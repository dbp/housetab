CREATE TABLE sets (
  id serial PRIMARY KEY,
  account_id integer NOT NULL references accounts(id),
  created_at timestamptz NOT NULL DEFAULT now(),
  typ text NOT NULL
  );

CREATE TABLE entry_sets (
  entry_id integer NOT NULL references entries(id),
  set_id integer NOT NULL references sets(id)
  );

ALTER TABLE entries ADD COLUMN archived timestamptz;
