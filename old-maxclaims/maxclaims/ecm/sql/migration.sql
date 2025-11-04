CREATE SCHEMA IF NOT EXISTS migration;


 CREATE TABLE IF NOT EXISTS migration.information (
    id BOOLEAN PRIMARY KEY DEFAULT TRUE, -- This is just so this table
                                         -- can be contstrained to one
                                         -- entry
    CHECK (id),
    current INTEGER NOT NULL DEFAULT -1, 
    path TEXT NOT NULL DEFAULT '/etc/postgresql/migration/'
 );
 
CREATE OR REPLACE FUNCTION migration.information ()
  RETURNS JSON LANGUAGE SQL AS $$
  SELECT to_json(i.*) FROM migration.information AS i ;
 $$;


CREATE OR REPLACE FUNCTION migration.default_information ()
 RETURNS JSON LANGUAGE SQL AS $$
 SELECT '
{
  "current" : -1, 
  "path" : "/etc/postgresql/migration/"
}'::json;
$$;

CREATE OR REPLACE FUNCTION migration.initialize
  (options json DEFAULT null::json)
RETURNS json LANGUAGE PLPGSQL AS $$
DECLARE
_current integer;
_path text;
default_information JSON := migration.default_information();
BEGIN

_current := COALESCE(options->'current',
                     (select current from migration.information)::text::json, 
                      default_information->'current');
_path := COALESCE(options->>'path',
                  (select path from migration.information),
                   default_information->>'path')::text;
BEGIN 
   INSERT INTO migration.information(current, path)
     VALUES (_current, _path);
EXCEPTION WHEN unique_violation THEN
   UPDATE migration.information 
   SET current = _current, 
       path = _path ;
END;

RETURN migration.information() ;
END;

$$;


CREATE OR REPLACE FUNCTION migration.read_file(file text)
  RETURNS text AS $$
    DECLARE
      content text;
      tmp text;
    BEGIN
      file := quote_literal(file);
      tmp := quote_ident(gen_random_uuid()::text);

      EXECUTE 'CREATE TEMP TABLE ' || tmp || ' (content text)';
      EXECUTE 'COPY ' || tmp || ' FROM ' || file;
      EXECUTE 'SELECT string_agg(content,chr(10)) FROM ' || tmp INTO content;
      EXECUTE 'DROP TABLE ' || tmp;

      RETURN content;
    END;
  $$ LANGUAGE plpgsql VOLATILE;

CREATE OR REPLACE FUNCTION migration.eval(text)
  RETURNS void LANGUAGE PLPGSQL AS $$
BEGIN
 EXECUTE $1;
END;
$$; 

CREATE OR REPLACE FUNCTION migration.run
 (number INTEGER, direction text DEFAULT 'up')
  RETURNS json LANGUAGE PLPGSQL AS $$
DECLARE 
  filename text;
  migration text;
BEGIN

 SELECT (migration.information())->>'path' 
          || '/' || $1 || '/' || $2 || '.sql' INTO filename ;

 SELECT migration.read_file(filename) INTO migration ;

 PERFORM migration.eval(migration); 

 RETURN json_build_object('filename', filename, 'migration', migration) ;

END;
$$;

CREATE OR REPLACE FUNCTION migration.migrate_database
 (migration INT default NULL::int)
RETURNS json LANGUAGE PLPGSQL AS $$
DECLARE 
 _info json := migration.information() ;
 _current integer;
 _next integer;
 _version integer;
 _direction text;
BEGIN

 PERFORM migration.initialize();

 _current := (_info->>'current')::int;
 _version := $1;

 -- RAISE NOTICE ' Trying current % to version %', _current, _version;

 -- Up
 IF (_version IS NULL OR (_version > _current AND _version >= 0)) THEN
   _next := _current + 1;
   _direction := 'up';

 -- Down  
  ELSIF (_version < _current) THEN 
  _next := _current;
  _direction := 'down';
ELSE 
  RETURN (SELECT migration.information());
END IF;

-- RAISE NOTICE 'So next is  % direction %', _next, _direction;

PERFORM migration.run(_next, _direction);

IF (_direction = 'down') THEN
 _next := _next - 1;
END IF;

PERFORM migration.initialize(json_build_object('current', _next));

IF ((_version IS NULL) OR ( _version != _next AND _version != _current AND _next >= 0)) THEN
 RETURN (SELECT migration.migrate_database(_version));
ELSE 
 RETURN (SELECT migration.information());
END IF;

EXCEPTION 
  WHEN undefined_file THEN
   IF (_version IS NULL) THEN
     RETURN (SELECT migration.information());
   ELSE 
    RAISE EXCEPTION 'Error %', SQLERRM USING ERRCODE = SQLSTATE ;
  END IF;
END; 
$$;
