--
-- PostgreSQL database dump
--

SET client_encoding = 'UTF8';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

--
-- Name: plpgsql; Type: PROCEDURAL LANGUAGE; Schema: -; Owner: postgres
--

CREATE PROCEDURAL LANGUAGE plpgsql;


ALTER PROCEDURAL LANGUAGE plpgsql OWNER TO postgres;

SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: app_resource; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE app_resource (
    app_resource_id integer NOT NULL,
    description text,
    code text
);


ALTER TABLE public.app_resource OWNER TO maxclaims;

--
-- Name: app_user; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE app_user (
    app_user_id integer NOT NULL,
    username text,
    password text,
    person_id integer,
    admin boolean DEFAULT false NOT NULL
);


ALTER TABLE public.app_user OWNER TO maxclaims;

--
-- Name: app_user_app_resource; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE app_user_app_resource (
    app_user_app_resource_id integer NOT NULL,
    app_user_id integer,
    app_resource_id integer
);


ALTER TABLE public.app_user_app_resource OWNER TO maxclaims;

--
-- Name: app_user_contract; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE app_user_contract (
    app_user_app_resource_id integer NOT NULL,
    app_user_id integer,
    contract_id integer
);


ALTER TABLE public.app_user_contract OWNER TO maxclaims;

--
-- Name: attachment; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE attachment (
    attachment_id integer NOT NULL,
    claim_id integer,
    app_user_id integer,
    date timestamp without time zone,
    file_description text,
    file_name text,
    file_type text,
    file bytea
);


ALTER TABLE public.attachment OWNER TO maxclaims;

--
-- Name: attachment_history; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE attachment_history (
    attachment_id integer NOT NULL,
    rec_version integer,
    person_id integer,
    file_name text,
    file_description text,
    content bytea,
    document_link_type text,
    document_link_id text
);


ALTER TABLE public.attachment_history OWNER TO maxclaims;

--
-- Name: automobile; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE automobile (
    automobile_id integer NOT NULL,
    policy_id integer,
    rec_version integer,
    risk_number integer,
    model_year timestamp without time zone,
    model_code text,
    make_code text,
    serial_number text,
    contract_id integer
);


ALTER TABLE public.automobile OWNER TO maxclaims;

--
-- Name: claim_check_expense(integer); Type: FUNCTION; Schema: public; Owner: maxclaims
--

CREATE FUNCTION claim_check_expense(integer) RETURNS numeric
    AS $_$select coalesce(sum(amount), 0) from claimtransaction where transaction_type_code = '4' and claim_id = $1;$_$
    LANGUAGE sql;


ALTER FUNCTION public.claim_check_expense(integer) OWNER TO maxclaims;

--
-- Name: claim_check_in_house(integer); Type: FUNCTION; Schema: public; Owner: maxclaims
--

CREATE FUNCTION claim_check_in_house(integer) RETURNS numeric
    AS $_$select coalesce(sum(amount), 0) from claimtransaction where transaction_type_code = '5' and claim_id = $1;$_$
    LANGUAGE sql;


ALTER FUNCTION public.claim_check_in_house(integer) OWNER TO maxclaims;

--
-- Name: claim_check_loss(integer); Type: FUNCTION; Schema: public; Owner: maxclaims
--

CREATE FUNCTION claim_check_loss(integer) RETURNS numeric
    AS $_$select coalesce(sum(amount), 0) from claimtransaction where transaction_type_code = '3' and claim_id = $1;$_$
    LANGUAGE sql;


ALTER FUNCTION public.claim_check_loss(integer) OWNER TO maxclaims;

--
-- Name: claim_incurred(integer); Type: FUNCTION; Schema: public; Owner: maxclaims
--

CREATE FUNCTION claim_incurred(integer) RETURNS numeric
    AS $_$ SELECT loss_total + expense_total + outstanding_reserve AS incurred from claim_transaction_bordereau where claim_id = $1$_$
    LANGUAGE sql;



ALTER FUNCTION public.claim_incurred(integer) OWNER TO maxclaims;

--
-- Name: claim_initial_reserve(integer); Type: FUNCTION; Schema: public; Owner: maxclaims
--

CREATE FUNCTION claim_initial_reserve(integer) RETURNS numeric
    AS $_$select coalesce(sum(amount), 0) from claimtransaction where transaction_type_code = '1' and claim_id = $1$_$
    LANGUAGE sql;


ALTER FUNCTION public.claim_initial_reserve(integer) OWNER TO maxclaims;

--
-- Name: claim_reserve_adjustment(integer); Type: FUNCTION; Schema: public; Owner: maxclaims
--

CREATE FUNCTION claim_reserve_adjustment(integer) RETURNS numeric
    AS $_$select coalesce(sum(amount), 0) from claimtransaction where transaction_type_code = '2' and claim_id = $1;$_$
    LANGUAGE sql;


ALTER FUNCTION public.claim_reserve_adjustment(integer) OWNER TO maxclaims;

--
-- Name: claim_salvage(integer); Type: FUNCTION; Schema: public; Owner: maxclaims
--

CREATE FUNCTION claim_salvage(integer) RETURNS numeric
    AS $_$select coalesce(sum(amount), 0) from claimtransaction where transaction_type_code = '7' and claim_id = $1;$_$
    LANGUAGE sql;


ALTER FUNCTION public.claim_salvage(integer) OWNER TO maxclaims;

--
-- Name: claim_status(integer); Type: FUNCTION; Schema: public; Owner: maxclaims
--

CREATE FUNCTION claim_status(integer) RETURNS text
    AS $_$SELECT codes.description FROM codes, claim 
        WHERE claim_id = $1 and codes.category = '4' 
             and codes.code = claim.status_code$_$
    LANGUAGE sql;



ALTER FUNCTION public.claim_status(integer) OWNER TO maxclaims;

--
-- Name: claim_subrogation(integer); Type: FUNCTION; Schema: public; Owner: maxclaims
--

CREATE FUNCTION claim_subrogation(integer) RETURNS numeric
    AS $_$select coalesce(sum(amount), 0) from claimtransaction where transaction_type_code = '6' and claim_id = $1;$_$
    LANGUAGE sql;


ALTER FUNCTION public.claim_subrogation(integer) OWNER TO maxclaims;

--
-- Name: claim; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE claim (
    claim_id integer DEFAULT nextval(('claim_CLAIM_ID_seq'::text)::regclass) NOT NULL,
    rec_version integer,
    policy_id integer,
    adjuster_id integer,
    date_of_loss timestamp without time zone,
    status_code text,
    loss_code text,
    cause_code text,
    modified integer,
    rev_date timestamp without time zone,
    open_date timestamp without time zone,
    close_date timestamp without time zone,
    risk_number integer,
    policy_type_code text,
    plantiff_id integer,
    notes text
);


ALTER TABLE public.claim OWNER TO maxclaims;

--
-- Name: claim_transaction_totals; Type: VIEW; Schema: public; Owner: maxclaims
--

CREATE VIEW claim_transaction_totals AS
    SELECT claim.claim_id, claim_initial_reserve(claim.claim_id) AS initial_reserve, claim_reserve_adjustment(claim.claim_id) AS reserve_adjustment, claim_check_loss(claim.claim_id) AS check_loss, claim_check_expense(claim.claim_id) AS check_expense, claim_check_in_house(claim.claim_id) AS check_in_house, claim_subrogation(claim.claim_id) AS subrogation, claim_salvage(claim.claim_id) AS salvage FROM claim ORDER BY claim.claim_id;


ALTER TABLE public.claim_transaction_totals OWNER TO maxclaims;

--
-- Name: claim_transaction_bordereau; Type: VIEW; Schema: public; Owner: maxclaims
--

CREATE VIEW claim_transaction_bordereau AS
    SELECT claim_transaction_totals.claim_id, claim_transaction_totals.initial_reserve, claim_transaction_totals.reserve_adjustment, claim_transaction_totals.check_loss, claim_transaction_totals.check_expense, claim_transaction_totals.check_in_house, claim_transaction_totals.subrogation, claim_transaction_totals.salvage, (claim_transaction_totals.check_loss - (claim_transaction_totals.salvage + claim_transaction_totals.subrogation)) AS loss_total, claim_transaction_totals.check_expense AS expense_total, (claim_transaction_totals.initial_reserve + claim_transaction_totals.reserve_adjustment) AS reserve_total, CASE claim_status(claim_transaction_totals.claim_id) WHEN 'Closed'::text THEN (0)::numeric ELSE ((claim_transaction_totals.initial_reserve + claim_transaction_totals.reserve_adjustment) - (((claim_transaction_totals.check_loss + claim_transaction_totals.check_expense) + claim_transaction_totals.check_in_house) - (claim_transaction_totals.salvage + claim_transaction_totals.subrogation))) END AS outstanding_reserve FROM claim_transaction_totals;


ALTER TABLE public.claim_transaction_bordereau OWNER TO maxclaims;

--
-- Name: claims_bordereau_totals; Type: VIEW; Schema: public; Owner: maxclaims
--

CREATE VIEW claims_bordereau_totals AS
    SELECT claim.claim_id, claim_transaction_bordereau.loss_total, claim_transaction_bordereau.check_expense AS expense_total, claim_transaction_bordereau.initial_reserve, claim_transaction_bordereau.reserve_adjustment, (claim_transaction_bordereau.loss_total + claim_transaction_bordereau.expense_total) AS total_paid, claim_transaction_bordereau.outstanding_reserve, claim_incurred(claim.claim_id) AS incurred FROM (claim LEFT JOIN claim_transaction_bordereau ON ((claim.claim_id = claim_transaction_bordereau.claim_id)));


ALTER TABLE public.claims_bordereau_totals OWNER TO maxclaims;

--
-- Name: codes; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE codes (
    category integer,
    code text,
    description text
);


ALTER TABLE public.codes OWNER TO maxclaims;

--
-- Name: contract; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE contract (
    contract_id integer DEFAULT nextval(('contract_CONTRACT_ID_seq'::text)::regclass) NOT NULL,
    contract_number text,
    contract_year timestamp without time zone,
    agency_id integer,
    insurance_company_id integer,
    policy_type_code text
);


ALTER TABLE public.contract OWNER TO maxclaims;

--
-- Name: person; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE person (
    person_id integer DEFAULT nextval(('person_PERSON_ID_seq'::text)::regclass) NOT NULL,
    parent_person_id integer,
    record_version integer,
    first_name text,
    last_name text,
    company_name text,
    address1 text,
    address2 text,
    city text,
    province_state_code text,
    postal_zip_code text,
    home_phone text,
    work_phone text,
    fax_phone text,
    cell_phone text,
    email_address text,
    birth_date timestamp without time zone,
    person_type_code text,
    date_added timestamp without time zone,
    date_modified timestamp without time zone,
    company_flag integer,
    user_id text,
    user_password text,
    postal_code text
);


ALTER TABLE public.person OWNER TO maxclaims;

--
-- Name: policy; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE policy (
    policy_id integer DEFAULT nextval(('policy_POLICY_ID_seq'::text)::regclass) NOT NULL,
    rec_version integer,
    agent_id integer,
    insured_id integer,
    effective_date timestamp without time zone,
    expiry_date timestamp without time zone,
    policy_type_code text,
    policy_number text,
    deductible numeric(20,2),
    company_id integer,
    underwriter_id integer,
    branch_id integer,
    sub_agent_id integer
);


ALTER TABLE public.policy OWNER TO maxclaims;

--
-- Name: automobile_bordereau; Type: VIEW; Schema: public; Owner: maxclaims
--

CREATE VIEW automobile_bordereau AS
    SELECT 'automobile'::text AS type, contract.contract_number, ((((sub_agent.first_name || ' '::text) || sub_agent.last_name) || ' '::text) || sub_agent.company_name) AS office, policy.policy_number, claim.claim_id, ((((insured.first_name || ' '::text) || insured.last_name) || ' '::text) || insured.company_name) AS insured_name, province.description AS province, claim.date_of_loss, claim.cause_code, claim.loss_code, policy.effective_date, 

b.loss_total AS loss_total, 
b.expense_total AS expense_total, 
b.initial_reserve AS initial_reserve, b.reserve_adjustment AS reserve_adjustment, b.outstanding_reserve AS outstanding_reserve, b.total_paid AS total_paid, b.incurred AS incurred, status.description AS status FROM (((contract JOIN automobile risk ON ((risk.contract_id = contract.contract_id))) JOIN policy ON ((policy.policy_id = risk.policy_id))) FULL JOIN person sub_agent ON ((policy.sub_agent_id = sub_agent.person_id))), (claim LEFT JOIN claims_bordereau_totals b ON ((b.claim_id = claim.claim_id))), person insured, codes province, codes status WHERE (((((((((status.category = 4) AND (claim.status_code = status.code)) AND (province.category = 2)) AND (insured.province_state_code = province.code)) AND (risk.contract_id = contract.contract_id)) AND (policy.policy_id = risk.policy_id)) AND (claim.policy_id = risk.policy_id)) AND (claim.risk_number = risk.risk_number)) AND (insured.person_id = policy.insured_id));


ALTER TABLE public.automobile_bordereau OWNER TO maxclaims;

--
-- Name: automobile_history; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE automobile_history (
    policy_id integer,
    rec_version integer,
    risk_number integer,
    model_year timestamp without time zone,
    model_code text,
    make_code text,
    serial_number text,
    contract_id integer
);


ALTER TABLE public.automobile_history OWNER TO maxclaims;

--
-- Name: claim_history; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE claim_history (
    claim_id integer,
    rec_version integer,
    policy_id integer,
    adjuster_id integer,
    date_of_loss timestamp without time zone,
    status_code text,
    loss_code text,
    cause_code text,
    loss_detail bytea,
    modified integer,
    rev_date timestamp without time zone,
    open_date timestamp without time zone,
    close_date timestamp without time zone,
    risk_number integer,
    policy_type_code text
);


ALTER TABLE public.claim_history OWNER TO maxclaims;

--
-- Name: claimtransaction; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE claimtransaction (
    transaction_id integer DEFAULT nextval(('claimtransaction_claimtransaction_id_seq'::text)::regclass) NOT NULL,
    rec_version integer,
    transaction_type_code text,
    transaction_date timestamp without time zone,
    claim_id integer,
    payee_id integer,
    check_number text,
    amount numeric(20,2),
    schemes_advance_number text,
    reference_number text,
    expense_code text,
    transaction_heading_code text
);


ALTER TABLE public.claimtransaction OWNER TO maxclaims;

--
-- Name: claim_transaction_total; Type: VIEW; Schema: public; Owner: maxclaims
--

CREATE VIEW claim_transaction_total AS
    SELECT claim.claim_id, COALESCE(sum(initial_reserve.amount), (0)::numeric) AS initial_reserve, COALESCE(sum(reserve_adjustment.amount), (0)::numeric) AS reserve_adjustment, COALESCE(sum(check_loss.amount), (0)::numeric) AS check_loss, COALESCE(sum(check_expense.amount), (0)::numeric) AS check_expense, COALESCE(sum(check_in_house.amount), (0)::numeric) AS check_in_house, COALESCE(sum(subrogation.amount), (0)::numeric) AS subrogation, COALESCE(sum(salvage.amount), (0)::numeric) AS salvage FROM (((((((claim LEFT JOIN claimtransaction initial_reserve ON (((claim.claim_id = initial_reserve.claim_id) AND (initial_reserve.transaction_type_code = '4'::text)))) LEFT JOIN claimtransaction reserve_adjustment ON (((claim.claim_id = reserve_adjustment.claim_id) AND (reserve_adjustment.transaction_type_code = '2'::text)))) LEFT JOIN claimtransaction check_loss ON (((claim.claim_id = check_loss.claim_id) AND (check_loss.transaction_type_code = '3'::text)))) LEFT JOIN claimtransaction check_expense ON (((claim.claim_id = check_expense.claim_id) AND (check_expense.transaction_type_code = '4'::text)))) LEFT JOIN claimtransaction check_in_house ON (((claim.claim_id = check_in_house.claim_id) AND (check_in_house.transaction_type_code = '5'::text)))) LEFT JOIN claimtransaction subrogation ON (((claim.claim_id = subrogation.claim_id) AND (subrogation.transaction_type_code = '6'::text)))) LEFT JOIN claimtransaction salvage ON (((claim.claim_id = salvage.claim_id) AND (salvage.transaction_type_code = '7'::text)))) GROUP BY claim.claim_id;


ALTER TABLE public.claim_transaction_total OWNER TO maxclaims;

--
-- Name: commercial; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE commercial (
    commercial_id integer NOT NULL,
    policy_id integer,
    rec_version integer,
    risk_number integer,
    industry_code text,
    coverage_code text,
    contract_id integer
);


ALTER TABLE public.commercial OWNER TO maxclaims;

--
-- Name: commercial_bordereau; Type: VIEW; Schema: public; Owner: maxclaims
--

CREATE VIEW commercial_bordereau AS
    SELECT 'commercial'::text AS type, contract.contract_number, ((((sub_agent.first_name || ' '::text) || sub_agent.last_name) || ' '::text) || sub_agent.company_name) AS office, policy.policy_number, claim.claim_id, ((((insured.first_name || ' '::text) || insured.last_name) || ' '::text) || insured.company_name) AS insured_name, province.description AS province, claim.date_of_loss, claim.cause_code, claim.loss_code, policy.effective_date, b.loss_total AS loss_total, b.expense_total AS expense_total, b.initial_reserve AS initial_reserve, b.reserve_adjustment AS reserve_adjustment, b.outstanding_reserve AS outstanding_reserve, b.total_paid AS total_paid, b.incurred AS incurred, status.description AS status FROM (((contract JOIN commercial risk ON ((risk.contract_id = contract.contract_id))) JOIN policy ON ((policy.policy_id = risk.policy_id))) FULL JOIN person sub_agent ON ((policy.sub_agent_id = sub_agent.person_id))), (claim LEFT JOIN claims_bordereau_totals b ON ((b.claim_id = claim.claim_id))), person insured, codes province, codes status WHERE (((((((((status.category = 4) AND (claim.status_code = status.code)) AND (province.category = 2)) AND (insured.province_state_code = province.code)) AND (risk.contract_id = contract.contract_id)) AND (policy.policy_id = risk.policy_id)) AND (claim.policy_id = risk.policy_id)) AND (claim.risk_number = risk.risk_number)) AND (insured.person_id = policy.insured_id));


ALTER TABLE public.commercial_bordereau OWNER TO maxclaims;

--
-- Name: habitational; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE habitational (
    habitational_id integer NOT NULL,
    policy_id integer,
    rec_version integer,
    risk_number integer,
    insured_value numeric,
    habitational_type_code text,
    coverage_code text,
    contract_id integer
);


ALTER TABLE public.habitational OWNER TO maxclaims;

--
-- Name: habitational_bordereau; Type: VIEW; Schema: public; Owner: maxclaims
--

CREATE VIEW habitational_bordereau AS
    SELECT 'habitational'::text AS type, contract.contract_number, ((((sub_agent.first_name || ' '::text) || sub_agent.last_name) || ' '::text) || sub_agent.company_name) AS office, policy.policy_number, claim.claim_id, ((((insured.first_name || ' '::text) || insured.last_name) || ' '::text) || insured.company_name) AS insured_name, province.description AS province, claim.date_of_loss, claim.cause_code, claim.loss_code, policy.effective_date, b.loss_total AS loss_total, b.expense_total AS expense_total, b.initial_reserve AS initial_reserve, b.reserve_adjustment AS reserve_adjustment, b.outstanding_reserve AS outstanding_reserve, b.total_paid AS total_paid, b.incurred AS incurred, status.description AS status FROM (((contract JOIN habitational risk ON ((risk.contract_id = contract.contract_id))) JOIN policy ON ((policy.policy_id = risk.policy_id))) FULL JOIN person sub_agent ON ((policy.sub_agent_id = sub_agent.person_id))), (claim LEFT JOIN claims_bordereau_totals b ON ((b.claim_id = claim.claim_id))), person insured, codes province, codes status WHERE (((((((((status.category = 4) AND (claim.status_code = status.code)) AND (province.category = 2)) AND (insured.province_state_code = province.code)) AND (risk.contract_id = contract.contract_id)) AND (policy.policy_id = risk.policy_id)) AND (claim.policy_id = risk.policy_id)) AND (claim.risk_number = risk.risk_number)) AND (insured.person_id = policy.insured_id));


ALTER TABLE public.habitational_bordereau OWNER TO maxclaims;

--
-- Name: marine; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE marine (
    marine_id integer NOT NULL,
    policy_id integer,
    rec_version integer,
    risk_number integer,
    model_year timestamp without time zone,
    model_code text,
    make_code text,
    serial_number text,
    contract_id integer
);


ALTER TABLE public.marine OWNER TO maxclaims;

--
-- Name: marine_bordereau; Type: VIEW; Schema: public; Owner: maxclaims
--

CREATE VIEW marine_bordereau AS
    SELECT 'marine'::text AS type, contract.contract_number, ((((sub_agent.first_name || ' '::text) || sub_agent.last_name) || ' '::text) || sub_agent.company_name) AS office, policy.policy_number, claim.claim_id, ((((insured.first_name || ' '::text) || insured.last_name) || ' '::text) || insured.company_name) AS insured_name, province.description AS province, claim.date_of_loss, claim.cause_code, claim.loss_code, policy.effective_date, b.loss_total AS loss_total, b.expense_total AS expense_total, b.initial_reserve AS initial_reserve, b.reserve_adjustment AS reserve_adjustment, b.outstanding_reserve AS outstanding_reserve, b.total_paid AS total_paid, b.incurred AS incurred, status.description AS status FROM (((contract JOIN marine risk ON ((risk.contract_id = contract.contract_id))) JOIN policy ON ((policy.policy_id = risk.policy_id))) FULL JOIN person sub_agent ON ((policy.sub_agent_id = sub_agent.person_id))), (claim LEFT JOIN claims_bordereau_totals b ON ((b.claim_id = claim.claim_id))), person insured, codes province, codes status WHERE (((((((((status.category = 4) AND (claim.status_code = status.code)) AND (province.category = 2)) AND (insured.province_state_code = province.code)) AND (risk.contract_id = contract.contract_id)) AND (policy.policy_id = risk.policy_id)) AND (claim.policy_id = risk.policy_id)) AND (claim.risk_number = risk.risk_number)) AND (insured.person_id = policy.insured_id));


ALTER TABLE public.marine_bordereau OWNER TO maxclaims;

--
-- Name: snowmobile; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE snowmobile (
    snowmobile_id integer NOT NULL,
    policy_id integer,
    rec_version integer,
    risk_number integer,
    model_year timestamp without time zone,
    model_code text,
    make_code text,
    serial_number text,
    contract_id integer
);


ALTER TABLE public.snowmobile OWNER TO maxclaims;

--
-- Name: snowmobile_bordereau; Type: VIEW; Schema: public; Owner: maxclaims
--

CREATE VIEW snowmobile_bordereau AS
    SELECT 'snowmobile'::text AS type, contract.contract_number, ((((sub_agent.first_name || ' '::text) || sub_agent.last_name) || ' '::text) || sub_agent.company_name) AS office, policy.policy_number, claim.claim_id, ((((insured.first_name || ' '::text) || insured.last_name) || ' '::text) || insured.company_name) AS insured_name, province.description AS province, claim.date_of_loss, claim.cause_code, claim.loss_code, policy.effective_date, b.loss_total AS loss_total, b.expense_total AS expense_total, b.initial_reserve AS initial_reserve, b.reserve_adjustment AS reserve_adjustment, b.outstanding_reserve AS outstanding_reserve, b.total_paid AS total_paid, b.incurred AS incurred, status.description AS status FROM (((contract JOIN snowmobile risk ON ((risk.contract_id = contract.contract_id))) JOIN policy ON ((policy.policy_id = risk.policy_id))) FULL JOIN person sub_agent ON ((policy.sub_agent_id = sub_agent.person_id))), (claim LEFT JOIN claims_bordereau_totals b ON ((b.claim_id = claim.claim_id))), person insured, codes province, codes status WHERE (((((((((status.category = 4) AND (claim.status_code = status.code)) AND (province.category = 2)) AND (insured.province_state_code = province.code)) AND (risk.contract_id = contract.contract_id)) AND (policy.policy_id = risk.policy_id)) AND (claim.policy_id = risk.policy_id)) AND (claim.risk_number = risk.risk_number)) AND (insured.person_id = policy.insured_id));


ALTER TABLE public.snowmobile_bordereau OWNER TO maxclaims;

--
-- Name: travel; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE travel (
    travel_id integer NOT NULL,
    rec_version integer,
    risk_number integer,
    coverage_code text,
    policy_id integer,
    contract_id integer
);


ALTER TABLE public.travel OWNER TO maxclaims;

--
-- Name: travel_bordereau; Type: VIEW; Schema: public; Owner: maxclaims
--

CREATE VIEW travel_bordereau AS
    SELECT 'travel'::text AS type, contract.contract_number, ((((sub_agent.first_name || ' '::text) || sub_agent.last_name) || ' '::text) || sub_agent.company_name) AS office, policy.policy_number, claim.claim_id, ((((insured.first_name || ' '::text) || insured.last_name) || ' '::text) || insured.company_name) AS insured_name, province.description AS province, claim.date_of_loss, claim.cause_code, claim.loss_code, policy.effective_date, b.loss_total AS loss_total, b.expense_total AS expense_total, b.initial_reserve AS initial_reserve, b.reserve_adjustment AS reserve_adjustment, b.outstanding_reserve AS outstanding_reserve, b.total_paid AS total_paid, b.incurred AS incurred, status.description AS status FROM (((contract JOIN travel risk ON ((risk.contract_id = contract.contract_id))) JOIN policy ON ((policy.policy_id = risk.policy_id))) FULL JOIN person sub_agent ON ((policy.sub_agent_id = sub_agent.person_id))), (claim LEFT JOIN claims_bordereau_totals b ON ((b.claim_id = claim.claim_id))), person insured, codes province, codes status WHERE (((((((((status.category = 4) AND (claim.status_code = status.code)) AND (province.category = 2)) AND (insured.province_state_code = province.code)) AND (risk.contract_id = contract.contract_id)) AND (policy.policy_id = risk.policy_id)) AND (claim.policy_id = risk.policy_id)) AND (claim.risk_number = risk.risk_number)) AND (insured.person_id = policy.insured_id));


ALTER TABLE public.travel_bordereau OWNER TO maxclaims;

--
-- Name: claims_bordereau; Type: VIEW; Schema: public; Owner: maxclaims
--

CREATE VIEW claims_bordereau AS
    SELECT COALESCE(automobile.type, habitational.type, commercial.type, marine.type, snowmobile.type, travel.type) AS type, COALESCE(automobile.contract_number, habitational.contract_number, commercial.contract_number, marine.contract_number, snowmobile.contract_number, travel.contract_number) AS contract_number, COALESCE(automobile.office, habitational.office, commercial.office, marine.office, snowmobile.office, travel.office) AS office, COALESCE(automobile.policy_number, habitational.policy_number, commercial.policy_number, marine.policy_number, snowmobile.policy_number, travel.policy_number) AS policy_number, COALESCE(automobile.claim_id, habitational.claim_id, commercial.claim_id, marine.claim_id, snowmobile.claim_id, travel.claim_id) AS claim_id, COALESCE(automobile.insured_name, habitational.insured_name, commercial.insured_name, marine.insured_name, snowmobile.insured_name, travel.insured_name) AS insured_name, COALESCE(automobile.province, habitational.province, commercial.province, marine.province, snowmobile.province, travel.province) AS province, COALESCE(automobile.date_of_loss, habitational.date_of_loss, commercial.date_of_loss, marine.date_of_loss, snowmobile.date_of_loss, travel.date_of_loss) AS date_of_loss, COALESCE(automobile.cause_code, habitational.cause_code, commercial.cause_code, marine.cause_code, snowmobile.cause_code, travel.cause_code) AS cause_code, COALESCE(automobile.loss_code, habitational.loss_code, commercial.loss_code, marine.loss_code, snowmobile.loss_code, travel.loss_code) AS loss_code, COALESCE(automobile.effective_date, habitational.effective_date, commercial.effective_date, marine.effective_date, snowmobile.effective_date, travel.effective_date) AS effective_date, COALESCE(automobile.loss_total, habitational.loss_total, commercial.loss_total, marine.loss_total, snowmobile.loss_total, travel.loss_total) AS loss_total, COALESCE(automobile.expense_total, habitational.expense_total, commercial.expense_total, marine.expense_total, snowmobile.expense_total, travel.expense_total) AS expense_total, COALESCE(automobile.initial_reserve, habitational.initial_reserve, commercial.initial_reserve, marine.initial_reserve, snowmobile.initial_reserve, travel.initial_reserve) AS initial_reserve, COALESCE(automobile.reserve_adjustment, habitational.reserve_adjustment, commercial.reserve_adjustment, marine.reserve_adjustment, snowmobile.reserve_adjustment, travel.reserve_adjustment) AS reserve_adjustment, COALESCE(automobile.outstanding_reserve, habitational.outstanding_reserve, commercial.outstanding_reserve, marine.outstanding_reserve, snowmobile.outstanding_reserve, travel.outstanding_reserve) AS outstanding_reserve, COALESCE(automobile.total_paid, habitational.total_paid, commercial.total_paid, marine.total_paid, snowmobile.total_paid, travel.total_paid) AS total_paid, COALESCE(automobile.incurred, habitational.incurred, commercial.incurred, marine.incurred, snowmobile.incurred, travel.incurred) AS incurred, COALESCE(automobile.status, habitational.status, commercial.status, marine.status, snowmobile.status, travel.status) AS status FROM ((((((claim FULL JOIN automobile_bordereau automobile ON ((claim.claim_id = automobile.claim_id))) FULL JOIN habitational_bordereau habitational ON ((claim.claim_id = habitational.claim_id))) FULL JOIN commercial_bordereau commercial ON ((claim.claim_id = commercial.claim_id))) FULL JOIN marine_bordereau marine ON ((claim.claim_id = marine.claim_id))) FULL JOIN snowmobile_bordereau snowmobile ON ((claim.claim_id = snowmobile.claim_id))) FULL JOIN travel_bordereau travel ON ((claim.claim_id = travel.claim_id)));


ALTER TABLE public.claims_bordereau OWNER TO maxclaims;

--
-- Name: claimtransaction_history; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE claimtransaction_history (
    transaction_id integer NOT NULL,
    rec_version integer,
    transaction_type_code text,
    transaction_date timestamp without time zone,
    claim_id integer,
    payee_id integer,
    check_number text,
    amount numeric(20,2),
    schemes_advance_number text,
    reference_number text,
    expense_code text
);


ALTER TABLE public.claimtransaction_history OWNER TO maxclaims;

--
-- Name: codelookup; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE codelookup (
    parent_table_name text,
    parent_column_name text,
    category integer
);


ALTER TABLE public.codelookup OWNER TO maxclaims;

--
-- Name: codelookup_history; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE codelookup_history (
    parent_table_name text,
    parent_column_name text,
    category integer
);


ALTER TABLE public.codelookup_history OWNER TO maxclaims;

--
-- Name: codes_history; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE codes_history (
    category integer,
    code text,
    description text
);


ALTER TABLE public.codes_history OWNER TO maxclaims;

--
-- Name: commercial_history; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE commercial_history (
    policy_id integer,
    rec_version integer,
    risk_number integer,
    industry_code text,
    coverage_code text,
    contract_id text
);


ALTER TABLE public.commercial_history OWNER TO maxclaims;

--
-- Name: contract_history; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE contract_history (
    contract_id integer NOT NULL,
    contract_number text,
    contract_year timestamp without time zone,
    agency_id integer,
    insurance_company_id integer,
    policy_type_code text
);


ALTER TABLE public.contract_history OWNER TO maxclaims;

--
-- Name: habitational_history; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE habitational_history (
    policy_id integer,
    rec_version integer,
    risk_number integer,
    insured_value numeric,
    habitational_type_code text,
    coverage_code text,
    contract_id integer
);


ALTER TABLE public.habitational_history OWNER TO maxclaims;

--
-- Name: history; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE history (
    history_id integer NOT NULL,
    object_type text,
    object_id integer,
    app_user_id integer,
    modification_time timestamp without time zone DEFAULT now(),
    history_data text
);


ALTER TABLE public.history OWNER TO maxclaims;

--
-- Name: person_history; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE person_history (
    person_id integer,
    parent_person_id integer,
    record_version integer,
    first_name text,
    last_name text,
    company_name text,
    address1 text,
    address2 text,
    city text,
    province_state_code text,
    postal_zip_code text,
    home_phone text,
    work_phone text,
    fax_phone text,
    cell_phone text,
    email_address text,
    birth_date timestamp without time zone,
    person_type_code text,
    date_added timestamp without time zone,
    date_modified timestamp without time zone,
    company_flag integer,
    user_id text,
    user_password text
);


ALTER TABLE public.person_history OWNER TO maxclaims;

--
-- Name: policy_history; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE policy_history (
    policy_id integer NOT NULL,
    rec_version integer,
    agent_id integer,
    insured_id text,
    effective_date timestamp without time zone,
    expiry_date timestamp without time zone,
    policy_type_code text,
    policy_number text,
    deductible numeric(20,2),
    company_id integer
);


ALTER TABLE public.policy_history OWNER TO maxclaims;

--
-- Name: snowmobile_history; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE snowmobile_history (
    policy_id integer,
    rec_version integer,
    risk_number integer,
    model_year timestamp without time zone,
    model_code text,
    make_code text,
    serial_number text,
    contract_id integer
);


ALTER TABLE public.snowmobile_history OWNER TO maxclaims;

--
-- Name: suspense; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE suspense (
    suspense_id integer NOT NULL,
    rec_version integer,
    person_id integer,
    suspense_type_code text,
    suspense_date timestamp without time zone,
    transaction_date timestamp without time zone,
    review_comments bytea
);


ALTER TABLE public.suspense OWNER TO maxclaims;

--
-- Name: suspense_history; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE suspense_history (
    suspense_id integer NOT NULL,
    rec_version integer,
    person_id integer,
    suspense_type_code text,
    suspense_date timestamp without time zone,
    transaction_date timestamp without time zone,
    review_comments bytea
);


ALTER TABLE public.suspense_history OWNER TO maxclaims;

--
-- Name: timecard; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE timecard (
    timecard_id integer NOT NULL,
    claim_id integer,
    app_user_id integer,
    date timestamp without time zone,
    minutes integer,
    notes text
);


ALTER TABLE public.timecard OWNER TO maxclaims;

--
-- Name: travel_history; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE travel_history (
    policy_id integer,
    rec_version integer,
    risk_number integer,
    coverage_code text,
    contract_id text
);


ALTER TABLE public.travel_history OWNER TO maxclaims;

--
-- Name: user_bordereau; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE user_bordereau (
    user_bordereau_id integer NOT NULL,
    app_user_id integer,
    title text
);


ALTER TABLE public.user_bordereau OWNER TO maxclaims;

--
-- Name: user_bordereau_contract; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE user_bordereau_contract (
    user_bordereau_contract_id integer NOT NULL,
    user_bordereau_id integer,
    contract_id integer
);


ALTER TABLE public.user_bordereau_contract OWNER TO maxclaims;

--
-- Name: userreport; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE userreport (
    user_report_id integer NOT NULL,
    user_id integer,
    report_description integer,
    report_date timestamp without time zone,
    report_content text
);


ALTER TABLE public.userreport OWNER TO maxclaims;

--
-- Name: userreport_history; Type: TABLE; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE TABLE userreport_history (
    user_report_id integer NOT NULL,
    user_id integer,
    report_description integer,
    report_date timestamp without time zone,
    report_content text
);


ALTER TABLE public.userreport_history OWNER TO maxclaims;

--
-- Name: claim_check_expense(integer, timestamp without time zone, timestamp without time zone); Type: FUNCTION; Schema: public; Owner: maxclaims
--

CREATE FUNCTION claim_check_expense(integer, timestamp without time zone, timestamp without time zone) RETURNS numeric
    AS $_$select coalesce(sum(amount), 0) from claimtransaction where transaction_type_code = '4' and claim_id = $1 and transaction_date >= $2 and transaction_date <= $3;$_$
    LANGUAGE sql;


ALTER FUNCTION public.claim_check_expense(integer, timestamp without time zone, timestamp without time zone) OWNER TO maxclaims;

--
-- Name: claim_check_in_house(integer, timestamp without time zone, timestamp without time zone); Type: FUNCTION; Schema: public; Owner: maxclaims
--

CREATE FUNCTION claim_check_in_house(integer, timestamp without time zone, timestamp without time zone) RETURNS numeric
    AS $_$select coalesce(sum(amount), 0) from claimtransaction where transaction_type_code = '5' and claim_id = $1 and transaction_date >= $2 and transaction_date <= $3;$_$
    LANGUAGE sql;


ALTER FUNCTION public.claim_check_in_house(integer, timestamp without time zone, timestamp without time zone) OWNER TO maxclaims;

--
-- Name: claim_check_loss(integer, timestamp without time zone, timestamp without time zone); Type: FUNCTION; Schema: public; Owner: maxclaims
--

CREATE FUNCTION claim_check_loss(integer, timestamp without time zone, timestamp without time zone) RETURNS numeric
    AS $_$select coalesce(sum(amount), 0) from claimtransaction where transaction_type_code = '3' and claim_id = $1 and transaction_date >= $2 and transaction_date <= $3;$_$
    LANGUAGE sql;


ALTER FUNCTION public.claim_check_loss(integer, timestamp without time zone, timestamp without time zone) OWNER TO maxclaims;

--
-- Name: claim_outstanding_reserve(integer); Type: FUNCTION; Schema: public; Owner: maxclaims
--

CREATE FUNCTION claim_outstanding_reserve(integer) RETURNS numeric
    AS $_$     SELECT 
   CASE claim_status(claim_id) 
               WHEN 'Closed' 
                  THEN 0 
               ELSE
                (initial_reserve + reserve_adjustment) -    
         ((check_loss + check_expense + check_in_house) 
            - (salvage + subrogation))
           END AS outstanding_reserve   
     from claim_transaction_total($1)$_$
    LANGUAGE sql;


ALTER FUNCTION public.claim_outstanding_reserve(integer) OWNER TO maxclaims;

--
-- Name: claim_salvage(integer, timestamp without time zone, timestamp without time zone); Type: FUNCTION; Schema: public; Owner: maxclaims
--

CREATE FUNCTION claim_salvage(integer, timestamp without time zone, timestamp without time zone) RETURNS numeric
    AS $_$select coalesce(sum(amount), 0) from claimtransaction where transaction_type_code = '7' and claim_id = $1 and transaction_date >= $2 and transaction_date <= $3;$_$
    LANGUAGE sql;


ALTER FUNCTION public.claim_salvage(integer, timestamp without time zone, timestamp without time zone) OWNER TO maxclaims;

--
-- Name: claim_subrogation(integer, timestamp without time zone, timestamp without time zone); Type: FUNCTION; Schema: public; Owner: maxclaims
--

CREATE FUNCTION claim_subrogation(integer, timestamp without time zone, timestamp without time zone) RETURNS numeric
    AS $_$select coalesce(sum(amount), 0) from claimtransaction where transaction_type_code = '6' and claim_id = $1 and transaction_date >= $2 and transaction_date <= $3;$_$
    LANGUAGE sql;


ALTER FUNCTION public.claim_subrogation(integer, timestamp without time zone, timestamp without time zone) OWNER TO maxclaims;

--
-- Name: claim_transaction_total(integer); Type: FUNCTION; Schema: public; Owner: maxclaims
--

CREATE FUNCTION claim_transaction_total(integer) RETURNS claim_transaction_total
    AS $_$ SELECT claim_id, 
   claim_initial_reserve(claim_id) AS initial_reserve, 
           claim_reserve_adjustment(claim_id) AS reserve_adjustment, 
           claim_check_loss(claim_id)  AS check_loss,
           claim_check_expense(claim_id)  AS check_expense, 
   claim_check_in_house(claim_id) AS check_in_house, 
           claim_subrogation(claim_id) AS subrogation,  
           claim_salvage(claim_id) AS salvage
   FROM claim where claim_id = $1$_$
    LANGUAGE sql;


ALTER FUNCTION public.claim_transaction_total(integer) OWNER TO maxclaims;

--
-- Name: app_resource_app_resource_id_seq; Type: SEQUENCE; Schema: public; Owner: maxclaims
--

CREATE SEQUENCE app_resource_app_resource_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.app_resource_app_resource_id_seq OWNER TO maxclaims;

--
-- Name: app_resource_app_resource_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: maxclaims
--

ALTER SEQUENCE app_resource_app_resource_id_seq OWNED BY app_resource.app_resource_id;


--
-- Name: app_user_app_resource_app_user_app_resource_id_seq; Type: SEQUENCE; Schema: public; Owner: maxclaims
--

CREATE SEQUENCE app_user_app_resource_app_user_app_resource_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.app_user_app_resource_app_user_app_resource_id_seq OWNER TO maxclaims;

--
-- Name: app_user_app_resource_app_user_app_resource_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: maxclaims
--

ALTER SEQUENCE app_user_app_resource_app_user_app_resource_id_seq OWNED BY app_user_app_resource.app_user_app_resource_id;


--
-- Name: app_user_app_user_id_seq; Type: SEQUENCE; Schema: public; Owner: maxclaims
--

CREATE SEQUENCE app_user_app_user_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.app_user_app_user_id_seq OWNER TO maxclaims;

--
-- Name: app_user_app_user_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: maxclaims
--

ALTER SEQUENCE app_user_app_user_id_seq OWNED BY app_user.app_user_id;


--
-- Name: app_user_contract_app_user_app_resource_id_seq; Type: SEQUENCE; Schema: public; Owner: maxclaims
--

CREATE SEQUENCE app_user_contract_app_user_app_resource_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.app_user_contract_app_user_app_resource_id_seq OWNER TO maxclaims;

--
-- Name: app_user_contract_app_user_app_resource_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: maxclaims
--

ALTER SEQUENCE app_user_contract_app_user_app_resource_id_seq OWNED BY app_user_contract.app_user_app_resource_id;


--
-- Name: attachment_attachment_id_seq; Type: SEQUENCE; Schema: public; Owner: maxclaims
--

CREATE SEQUENCE attachment_attachment_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.attachment_attachment_id_seq OWNER TO maxclaims;

--
-- Name: attachment_attachment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: maxclaims
--

ALTER SEQUENCE attachment_attachment_id_seq OWNED BY attachment.attachment_id;


--
-- Name: automobile_automobile_id_seq; Type: SEQUENCE; Schema: public; Owner: maxclaims
--

CREATE SEQUENCE automobile_automobile_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.automobile_automobile_id_seq OWNER TO maxclaims;

--
-- Name: automobile_automobile_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: maxclaims
--

ALTER SEQUENCE automobile_automobile_id_seq OWNED BY automobile.automobile_id;


--
-- Name: claim_claim_id_seq; Type: SEQUENCE; Schema: public; Owner: maxclaims
--

CREATE SEQUENCE claim_claim_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.claim_claim_id_seq OWNER TO maxclaims;

--
-- Name: claimtransaction_claimtransaction_id_seq; Type: SEQUENCE; Schema: public; Owner: maxclaims
--

CREATE SEQUENCE claimtransaction_claimtransaction_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.claimtransaction_claimtransaction_id_seq OWNER TO maxclaims;

--
-- Name: commercial_commercial_id_seq; Type: SEQUENCE; Schema: public; Owner: maxclaims
--

CREATE SEQUENCE commercial_commercial_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.commercial_commercial_id_seq OWNER TO maxclaims;

--
-- Name: commercial_commercial_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: maxclaims
--

ALTER SEQUENCE commercial_commercial_id_seq OWNED BY commercial.commercial_id;


--
-- Name: contract_contract_id_seq; Type: SEQUENCE; Schema: public; Owner: maxclaims
--

CREATE SEQUENCE contract_contract_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.contract_contract_id_seq OWNER TO maxclaims;

--
-- Name: habitational_habitational_id_seq; Type: SEQUENCE; Schema: public; Owner: maxclaims
--

CREATE SEQUENCE habitational_habitational_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.habitational_habitational_id_seq OWNER TO maxclaims;

--
-- Name: habitational_habitational_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: maxclaims
--

ALTER SEQUENCE habitational_habitational_id_seq OWNED BY habitational.habitational_id;


--
-- Name: history_history_id_seq; Type: SEQUENCE; Schema: public; Owner: maxclaims
--

CREATE SEQUENCE history_history_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.history_history_id_seq OWNER TO maxclaims;

--
-- Name: history_history_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: maxclaims
--

ALTER SEQUENCE history_history_id_seq OWNED BY history.history_id;


--
-- Name: marine_marine_id_seq; Type: SEQUENCE; Schema: public; Owner: maxclaims
--

CREATE SEQUENCE marine_marine_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.marine_marine_id_seq OWNER TO maxclaims;

--
-- Name: marine_marine_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: maxclaims
--

ALTER SEQUENCE marine_marine_id_seq OWNED BY marine.marine_id;


--
-- Name: person_person_id_seq; Type: SEQUENCE; Schema: public; Owner: maxclaims
--

CREATE SEQUENCE person_person_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.person_person_id_seq OWNER TO maxclaims;

--
-- Name: policy_policy_id_seq; Type: SEQUENCE; Schema: public; Owner: maxclaims
--

CREATE SEQUENCE policy_policy_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.policy_policy_id_seq OWNER TO maxclaims;

--
-- Name: snowmobile_snowmobile_id_seq; Type: SEQUENCE; Schema: public; Owner: maxclaims
--

CREATE SEQUENCE snowmobile_snowmobile_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.snowmobile_snowmobile_id_seq OWNER TO maxclaims;

--
-- Name: snowmobile_snowmobile_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: maxclaims
--

ALTER SEQUENCE snowmobile_snowmobile_id_seq OWNED BY snowmobile.snowmobile_id;


--
-- Name: timecard_timecard_id_seq; Type: SEQUENCE; Schema: public; Owner: maxclaims
--

CREATE SEQUENCE timecard_timecard_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.timecard_timecard_id_seq OWNER TO maxclaims;

--
-- Name: timecard_timecard_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: maxclaims
--

ALTER SEQUENCE timecard_timecard_id_seq OWNED BY timecard.timecard_id;


--
-- Name: travel_travel_id_seq; Type: SEQUENCE; Schema: public; Owner: maxclaims
--

CREATE SEQUENCE travel_travel_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.travel_travel_id_seq OWNER TO maxclaims;

--
-- Name: travel_travel_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: maxclaims
--

ALTER SEQUENCE travel_travel_id_seq OWNED BY travel.travel_id;


--
-- Name: user_bordereau_contract_user_bordereau_contract_id_seq; Type: SEQUENCE; Schema: public; Owner: maxclaims
--

CREATE SEQUENCE user_bordereau_contract_user_bordereau_contract_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.user_bordereau_contract_user_bordereau_contract_id_seq OWNER TO maxclaims;

--
-- Name: user_bordereau_contract_user_bordereau_contract_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: maxclaims
--

ALTER SEQUENCE user_bordereau_contract_user_bordereau_contract_id_seq OWNED BY user_bordereau_contract.user_bordereau_contract_id;


--
-- Name: user_bordereau_user_bordereau_id_seq; Type: SEQUENCE; Schema: public; Owner: maxclaims
--

CREATE SEQUENCE user_bordereau_user_bordereau_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.user_bordereau_user_bordereau_id_seq OWNER TO maxclaims;

--
-- Name: user_bordereau_user_bordereau_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: maxclaims
--

ALTER SEQUENCE user_bordereau_user_bordereau_id_seq OWNED BY user_bordereau.user_bordereau_id;


--
-- Name: app_resource_id; Type: DEFAULT; Schema: public; Owner: maxclaims
--

ALTER TABLE app_resource ALTER COLUMN app_resource_id SET DEFAULT nextval('app_resource_app_resource_id_seq'::regclass);


--
-- Name: app_user_id; Type: DEFAULT; Schema: public; Owner: maxclaims
--

ALTER TABLE app_user ALTER COLUMN app_user_id SET DEFAULT nextval('app_user_app_user_id_seq'::regclass);


--
-- Name: app_user_app_resource_id; Type: DEFAULT; Schema: public; Owner: maxclaims
--

ALTER TABLE app_user_app_resource ALTER COLUMN app_user_app_resource_id SET DEFAULT nextval('app_user_app_resource_app_user_app_resource_id_seq'::regclass);


--
-- Name: app_user_app_resource_id; Type: DEFAULT; Schema: public; Owner: maxclaims
--

ALTER TABLE app_user_contract ALTER COLUMN app_user_app_resource_id SET DEFAULT nextval('app_user_contract_app_user_app_resource_id_seq'::regclass);


--
-- Name: attachment_id; Type: DEFAULT; Schema: public; Owner: maxclaims
--

ALTER TABLE attachment ALTER COLUMN attachment_id SET DEFAULT nextval('attachment_attachment_id_seq'::regclass);


--
-- Name: automobile_id; Type: DEFAULT; Schema: public; Owner: maxclaims
--

ALTER TABLE automobile ALTER COLUMN automobile_id SET DEFAULT nextval('automobile_automobile_id_seq'::regclass);


--
-- Name: commercial_id; Type: DEFAULT; Schema: public; Owner: maxclaims
--

ALTER TABLE commercial ALTER COLUMN commercial_id SET DEFAULT nextval('commercial_commercial_id_seq'::regclass);


--
-- Name: habitational_id; Type: DEFAULT; Schema: public; Owner: maxclaims
--

ALTER TABLE habitational ALTER COLUMN habitational_id SET DEFAULT nextval('habitational_habitational_id_seq'::regclass);


--
-- Name: history_id; Type: DEFAULT; Schema: public; Owner: maxclaims
--

ALTER TABLE history ALTER COLUMN history_id SET DEFAULT nextval('history_history_id_seq'::regclass);


--
-- Name: marine_id; Type: DEFAULT; Schema: public; Owner: maxclaims
--

ALTER TABLE marine ALTER COLUMN marine_id SET DEFAULT nextval('marine_marine_id_seq'::regclass);


--
-- Name: snowmobile_id; Type: DEFAULT; Schema: public; Owner: maxclaims
--

ALTER TABLE snowmobile ALTER COLUMN snowmobile_id SET DEFAULT nextval('snowmobile_snowmobile_id_seq'::regclass);


--
-- Name: timecard_id; Type: DEFAULT; Schema: public; Owner: maxclaims
--

ALTER TABLE timecard ALTER COLUMN timecard_id SET DEFAULT nextval('timecard_timecard_id_seq'::regclass);


--
-- Name: travel_id; Type: DEFAULT; Schema: public; Owner: maxclaims
--

ALTER TABLE travel ALTER COLUMN travel_id SET DEFAULT nextval('travel_travel_id_seq'::regclass);


--
-- Name: user_bordereau_id; Type: DEFAULT; Schema: public; Owner: maxclaims
--

ALTER TABLE user_bordereau ALTER COLUMN user_bordereau_id SET DEFAULT nextval('user_bordereau_user_bordereau_id_seq'::regclass);


--
-- Name: user_bordereau_contract_id; Type: DEFAULT; Schema: public; Owner: maxclaims
--

ALTER TABLE user_bordereau_contract ALTER COLUMN user_bordereau_contract_id SET DEFAULT nextval('user_bordereau_contract_user_bordereau_contract_id_seq'::regclass);


--
-- Name: app_resource_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY app_resource
    ADD CONSTRAINT app_resource_pkey PRIMARY KEY (app_resource_id);


--
-- Name: app_user_app_resource_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY app_user_app_resource
    ADD CONSTRAINT app_user_app_resource_pkey PRIMARY KEY (app_user_app_resource_id);


--
-- Name: app_user_contract_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY app_user_contract
    ADD CONSTRAINT app_user_contract_pkey PRIMARY KEY (app_user_app_resource_id);


--
-- Name: app_user_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY app_user
    ADD CONSTRAINT app_user_pkey PRIMARY KEY (app_user_id);


--
-- Name: attachment_history_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY attachment_history
    ADD CONSTRAINT attachment_history_pkey PRIMARY KEY (attachment_id);


--
-- Name: attachment_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY attachment
    ADD CONSTRAINT attachment_pkey PRIMARY KEY (attachment_id);


--
-- Name: automobile_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY automobile
    ADD CONSTRAINT automobile_pkey PRIMARY KEY (automobile_id);


--
-- Name: automobile_risk_number_key; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY automobile
    ADD CONSTRAINT automobile_risk_number_key UNIQUE (risk_number, policy_id);


--
-- Name: claim_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY claim
    ADD CONSTRAINT claim_pkey PRIMARY KEY (claim_id);


--
-- Name: claimtransaction_history_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY claimtransaction_history
    ADD CONSTRAINT claimtransaction_history_pkey PRIMARY KEY (transaction_id);


--
-- Name: claimtransaction_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY claimtransaction
    ADD CONSTRAINT claimtransaction_pkey PRIMARY KEY (transaction_id);


--
-- Name: commercial_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY commercial
    ADD CONSTRAINT commercial_pkey PRIMARY KEY (commercial_id);


--
-- Name: commercial_risk_number_key; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY commercial
    ADD CONSTRAINT commercial_risk_number_key UNIQUE (risk_number, policy_id);


--
-- Name: commercial_risk_number_key1; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY commercial
    ADD CONSTRAINT commercial_risk_number_key1 UNIQUE (risk_number, contract_id, policy_id);


--
-- Name: contract_history_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY contract_history
    ADD CONSTRAINT contract_history_pkey PRIMARY KEY (contract_id);


--
-- Name: contract_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY contract
    ADD CONSTRAINT contract_pkey PRIMARY KEY (contract_id);


--
-- Name: habitational_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY habitational
    ADD CONSTRAINT habitational_pkey PRIMARY KEY (habitational_id);


--
-- Name: habitational_risk_number_key; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY habitational
    ADD CONSTRAINT habitational_risk_number_key UNIQUE (risk_number, policy_id);


--
-- Name: history_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY history
    ADD CONSTRAINT history_pkey PRIMARY KEY (history_id);


--
-- Name: marine_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY marine
    ADD CONSTRAINT marine_pkey PRIMARY KEY (marine_id);


--
-- Name: marine_risk_number_key; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY marine
    ADD CONSTRAINT marine_risk_number_key UNIQUE (risk_number, policy_id);


--
-- Name: person_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY person
    ADD CONSTRAINT person_pkey PRIMARY KEY (person_id);


--
-- Name: policy_history_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY policy_history
    ADD CONSTRAINT policy_history_pkey PRIMARY KEY (policy_id);


--
-- Name: policy_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY policy
    ADD CONSTRAINT policy_pkey PRIMARY KEY (policy_id);


--
-- Name: snowmobile_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY snowmobile
    ADD CONSTRAINT snowmobile_pkey PRIMARY KEY (snowmobile_id);


--
-- Name: snowmobile_risk_number_key; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY snowmobile
    ADD CONSTRAINT snowmobile_risk_number_key UNIQUE (risk_number, policy_id);


--
-- Name: suspense_history_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY suspense_history
    ADD CONSTRAINT suspense_history_pkey PRIMARY KEY (suspense_id);


--
-- Name: suspense_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY suspense
    ADD CONSTRAINT suspense_pkey PRIMARY KEY (suspense_id);


--
-- Name: timecard_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY timecard
    ADD CONSTRAINT timecard_pkey PRIMARY KEY (timecard_id);


--
-- Name: travel_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY travel
    ADD CONSTRAINT travel_pkey PRIMARY KEY (travel_id);


--
-- Name: travel_risk_number_key; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY travel
    ADD CONSTRAINT travel_risk_number_key UNIQUE (risk_number, policy_id);


--
-- Name: user_bordereau_contract_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY user_bordereau_contract
    ADD CONSTRAINT user_bordereau_contract_pkey PRIMARY KEY (user_bordereau_contract_id);


--
-- Name: user_bordereau_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY user_bordereau
    ADD CONSTRAINT user_bordereau_pkey PRIMARY KEY (user_bordereau_id);


--
-- Name: userreport_history_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY userreport_history
    ADD CONSTRAINT userreport_history_pkey PRIMARY KEY (user_report_id);


--
-- Name: userreport_pkey; Type: CONSTRAINT; Schema: public; Owner: maxclaims; Tablespace: 
--

ALTER TABLE ONLY userreport
    ADD CONSTRAINT userreport_pkey PRIMARY KEY (user_report_id);


--
-- Name: claim_number; Type: INDEX; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE INDEX claim_number ON claimtransaction USING btree (claim_id);


--
-- Name: claimtransaction_date; Type: INDEX; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE INDEX claimtransaction_date ON claimtransaction USING btree (transaction_date);


--
-- Name: codes_c; Type: INDEX; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE INDEX codes_c ON codes USING btree (category);


--
-- Name: codes_cd; Type: INDEX; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE INDEX codes_cd ON codes USING btree (code);


--
-- Name: contract_agent; Type: INDEX; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE INDEX contract_agent ON contract USING btree (agency_id);


--
-- Name: contract_number; Type: INDEX; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE INDEX contract_number ON contract USING btree (contract_number);


--
-- Name: idex_1; Type: INDEX; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE INDEX idex_1 ON claimtransaction USING btree (transaction_type_code, claim_id);


--
-- Name: risk_number; Type: INDEX; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE INDEX risk_number ON claim USING btree (risk_number);


--
-- Name: transaction_type_key; Type: INDEX; Schema: public; Owner: maxclaims; Tablespace: 
--

CREATE INDEX transaction_type_key ON claimtransaction USING btree (transaction_type_code);


--
-- Name: $1; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY policy
    ADD CONSTRAINT "$1" FOREIGN KEY (agent_id) REFERENCES person(person_id);


--
-- Name: $1; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY contract
    ADD CONSTRAINT "$1" FOREIGN KEY (agency_id) REFERENCES person(person_id);


--
-- Name: $1; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY claim
    ADD CONSTRAINT "$1" FOREIGN KEY (policy_id) REFERENCES policy(policy_id);


--
-- Name: $1; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY commercial
    ADD CONSTRAINT "$1" FOREIGN KEY (policy_id) REFERENCES policy(policy_id);


--
-- Name: $1; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY habitational
    ADD CONSTRAINT "$1" FOREIGN KEY (policy_id) REFERENCES policy(policy_id);


--
-- Name: $1; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY marine
    ADD CONSTRAINT "$1" FOREIGN KEY (policy_id) REFERENCES policy(policy_id);


--
-- Name: $1; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY automobile
    ADD CONSTRAINT "$1" FOREIGN KEY (policy_id) REFERENCES policy(policy_id);


--
-- Name: $1; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY snowmobile
    ADD CONSTRAINT "$1" FOREIGN KEY (policy_id) REFERENCES policy(policy_id);


--
-- Name: $1; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY claimtransaction
    ADD CONSTRAINT "$1" FOREIGN KEY (claim_id) REFERENCES claim(claim_id);


--
-- Name: $1; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY app_user
    ADD CONSTRAINT "$1" FOREIGN KEY (person_id) REFERENCES person(person_id);


--
-- Name: $1; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY app_user_app_resource
    ADD CONSTRAINT "$1" FOREIGN KEY (app_user_id) REFERENCES app_user(app_user_id);


--
-- Name: $1; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY travel
    ADD CONSTRAINT "$1" FOREIGN KEY (policy_id) REFERENCES policy(policy_id);


--
-- Name: $1; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY app_user_contract
    ADD CONSTRAINT "$1" FOREIGN KEY (app_user_id) REFERENCES app_user(app_user_id);


--
-- Name: $1; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY user_bordereau
    ADD CONSTRAINT "$1" FOREIGN KEY (app_user_id) REFERENCES app_user(app_user_id);


--
-- Name: $1; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY user_bordereau_contract
    ADD CONSTRAINT "$1" FOREIGN KEY (user_bordereau_id) REFERENCES user_bordereau(user_bordereau_id);


--
-- Name: $2; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY policy
    ADD CONSTRAINT "$2" FOREIGN KEY (insured_id) REFERENCES person(person_id);


--
-- Name: $2; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY contract
    ADD CONSTRAINT "$2" FOREIGN KEY (insurance_company_id) REFERENCES person(person_id);


--
-- Name: $2; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY claim
    ADD CONSTRAINT "$2" FOREIGN KEY (adjuster_id) REFERENCES person(person_id);


--
-- Name: $2; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY commercial
    ADD CONSTRAINT "$2" FOREIGN KEY (contract_id) REFERENCES contract(contract_id);


--
-- Name: $2; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY habitational
    ADD CONSTRAINT "$2" FOREIGN KEY (contract_id) REFERENCES contract(contract_id);


--
-- Name: $2; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY marine
    ADD CONSTRAINT "$2" FOREIGN KEY (contract_id) REFERENCES contract(contract_id);


--
-- Name: $2; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY automobile
    ADD CONSTRAINT "$2" FOREIGN KEY (contract_id) REFERENCES contract(contract_id);


--
-- Name: $2; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY snowmobile
    ADD CONSTRAINT "$2" FOREIGN KEY (contract_id) REFERENCES contract(contract_id);


--
-- Name: $2; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY claimtransaction
    ADD CONSTRAINT "$2" FOREIGN KEY (payee_id) REFERENCES person(person_id);


--
-- Name: $2; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY app_user_app_resource
    ADD CONSTRAINT "$2" FOREIGN KEY (app_resource_id) REFERENCES app_resource(app_resource_id);


--
-- Name: $2; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY travel
    ADD CONSTRAINT "$2" FOREIGN KEY (contract_id) REFERENCES contract(contract_id);


--
-- Name: $2; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY app_user_contract
    ADD CONSTRAINT "$2" FOREIGN KEY (contract_id) REFERENCES contract(contract_id);


--
-- Name: $2; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY user_bordereau_contract
    ADD CONSTRAINT "$2" FOREIGN KEY (contract_id) REFERENCES contract(contract_id);


--
-- Name: $2; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY claim_history

    ADD CONSTRAINT "$2" FOREIGN KEY (adjuster_id) REFERENCES person(person_id);


--
-- Name: $3; Type: FK CONSTRAINT; Schema: public; Owner: maxclaimsclaim
--

ALTER TABLE ONLY policy
    ADD CONSTRAINT "$3" FOREIGN KEY (company_id) REFERENCES person(person_id);


--
-- Name: $3; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY claim
    ADD CONSTRAINT "$3" FOREIGN KEY (plantiff_id) REFERENCES person(person_id);


--
-- Name: $4; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY policy
    ADD CONSTRAINT "$4" FOREIGN KEY (underwriter_id) REFERENCES person(person_id);


--
-- Name: $5; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY policy
    ADD CONSTRAINT "$5" FOREIGN KEY (branch_id) REFERENCES person(person_id);


--
-- Name: $6; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY policy
    ADD CONSTRAINT "$6" FOREIGN KEY (sub_agent_id) REFERENCES person(person_id);


--
-- Name: attachment_app_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY attachment
    ADD CONSTRAINT attachment_app_user_id_fkey FOREIGN KEY (app_user_id) REFERENCES app_user(app_user_id);


--
-- Name: attachment_claim_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY attachment
    ADD CONSTRAINT attachment_claim_id_fkey FOREIGN KEY (claim_id) REFERENCES claim(claim_id);


--
-- Name: history_app_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY history
    ADD CONSTRAINT history_app_user_id_fkey FOREIGN KEY (app_user_id) REFERENCES app_user(app_user_id);


--
-- Name: timecard_app_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY timecard
    ADD CONSTRAINT timecard_app_user_id_fkey FOREIGN KEY (app_user_id) REFERENCES app_user(app_user_id);


--
-- Name: timecard_claim_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: maxclaims
--

ALTER TABLE ONLY timecard
    ADD CONSTRAINT timecard_claim_id_fkey FOREIGN KEY (claim_id) REFERENCES claim(claim_id);


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

