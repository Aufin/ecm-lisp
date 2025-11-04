CREATE OR REPLACE FUNCTION diary_entry_deadline(diary_entry)
RETURNS DATE LANGUAGE SQL AS $$
 SELECT COALESCE (
   (SELECT MAX(defer_date)
    FROM defer_diary_entry WHERE diary_entry_id = $1.diary_entry_id),
   $1.action_date);
$$;

CREATE OR REPLACE FUNCTION diary_entry_is_outstanding(diary_entry)
RETURNS BOOLEAN LANGUAGE SQL AS $$

 SELECT (now() >= diary_entry_deadline($1) AND NOT $1.processed);

$$;
