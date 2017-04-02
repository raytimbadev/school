DROP TABLE IF EXISTS item_reservation;
DROP TABLE IF EXISTS item;

CREATE TABLE item (
    id SERIAL PRIMARY KEY,
    flight_number INTEGER NOT NULL,
    price INTEGER NOT NULL,
    customer_id INTEGER -- ghetto foreign key
);

CREATE TABLE item_reservation (
    item_id INTEGER NOT NULL REFERENCES item ( id ) ON DELETE CASCADE,
    customer_id INTEGER NOT NULL
);
