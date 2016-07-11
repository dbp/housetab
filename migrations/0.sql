CREATE TABLE accounts (
    id serial PRIMARY KEY,
    name text NOT NULL,
    email text NOT NULL,
    password bytea NOT NULL,
    salt bytea NOT NULL,
    tutorial_active boolean NOT NULL,
    record_history boolean NOT NULL
);

CREATE TABLE entries (
    id serial PRIMARY KEY,
    account_id integer NOT NULL,
    who integer NOT NULL,
    what text NOT NULL,
    category text NOT NULL,
    date timestamp with time zone NOT NULL,
    howmuch double precision NOT NULL,
    whopays integer[] NOT NULL
);

CREATE TABLE log (
    id serial PRIMARY KEY,
    account_id integer NOT NULL,
    type text NOT NULL,
    who_old integer,
    who_new integer,
    what_old text,
    what_new text,
    category_old text,
    category_new text,
    date_old timestamp with time zone,
    date_new timestamp with time zone,
    howmuch_old double precision,
    howmuch_new double precision,
    whopays_old integer[],
    whopays_new integer[]
);


CREATE TABLE migrations (
    name text NOT NULL,
    run_at timestamp with time zone DEFAULT now() NOT NULL
);

CREATE TABLE persons (
    id serial PRIMARY KEY,
    account_id integer NOT NULL,
    name text NOT NULL
);

CREATE TABLE shares (
    id serial PRIMARY KEY,
    person_id integer NOT NULL,
    start timestamp with time zone NOT NULL,
    value double precision NOT NULL
);

ALTER TABLE ONLY accounts
    ADD CONSTRAINT accounts_name_key UNIQUE (name);


ALTER TABLE ONLY entries
    ADD CONSTRAINT entries_account_id_fkey FOREIGN KEY (account_id) REFERENCES accounts(id);

ALTER TABLE ONLY entries
    ADD CONSTRAINT entries_who_fkey FOREIGN KEY (who) REFERENCES persons(id);


ALTER TABLE ONLY log
    ADD CONSTRAINT log_account_id_fkey FOREIGN KEY (account_id) REFERENCES accounts(id);

ALTER TABLE ONLY log
    ADD CONSTRAINT log_who_new_fkey FOREIGN KEY (who_new) REFERENCES persons(id);


ALTER TABLE ONLY log
    ADD CONSTRAINT log_who_old_fkey FOREIGN KEY (who_old) REFERENCES persons(id);

ALTER TABLE ONLY persons
    ADD CONSTRAINT persons_account_id_fkey FOREIGN KEY (account_id) REFERENCES accounts(id);


ALTER TABLE ONLY shares
    ADD CONSTRAINT shares_person_id_fkey FOREIGN KEY (person_id) REFERENCES persons(id);
