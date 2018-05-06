DELETE FROM users;
DELETE FROM books;


INSERT INTO users (email, password) VALUES ('jonh@hotmail.com', 'abc123');
INSERT INTO books (title, author, link, progression) VALUES ('book1', 'jean', 'google.com', 10);