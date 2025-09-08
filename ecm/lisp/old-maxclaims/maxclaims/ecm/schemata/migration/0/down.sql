ALTER TABLE contract DROP COLUMN syndicate_number;
ALTER TABLE contract DROP COLUMN year_of_account;

DROP FUNCTION claim_status(integer, timestamp);
DROP FUNCTION claim_status(claim, timestamp);
