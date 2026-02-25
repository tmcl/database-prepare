create table if not exists users (
  id integer primary key not null,
  name text not null,
  email text not null unique,
  created_at timestamp not null default (datetime())
);
