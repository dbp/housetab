DROP TABLE log;

CREATE TABLE emails (
    id serial PRIMARY KEY,
    account_id integer NOT NULL REFERENCES accounts(id),
    email text NOT NULL,
    verified_at timestamptz NOT NULL
);

CREATE TABLE authentications (
    id serial PRIMARY KEY,
    created_at timestamptz NOT NULL DEFAULT now(),
    account_id integer NOT NULL REFERENCES accounts(id),
    email_id integer NOT NULL REFERENCES emails(id),
    token text NOT NULL DEFAULT md5(random()::text)
);

ALTER TABLE accounts DROP COLUMN password;
ALTER TABLE accounts DROP COLUMN salt;
ALTER TABLE accounts DROP COLUMN record_history;
ALTER TABLE accounts DROP COLUMN tutorial_active;

INSERT INTO emails (account_id, email, verified_at) (select id, email, now() from accounts);

ALTER TABLE accounts DROP COLUMN email;
