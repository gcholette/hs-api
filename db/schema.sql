CREATE TABLE users (
    id serial NOT NULL, 
    email varchar, 
    password varchar 
);

CREATE TABLE books (
    id serial PRIMARY KEY, 
    title varchar, 
    author varchar, 
    link varchar
);