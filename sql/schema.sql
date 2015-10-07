DROP TABLE IF EXISTS flight_reservation;
DROP TABLE IF EXISTS room_reservation;
DROP TABLE IF EXISTS car_reservation;
DROP TABLE IF EXISTS flight;
DROP TABLE IF EXISTS room;
DROP TABLE IF EXISTS car;
DROP TABLE IF EXISTS customer;
DROP TABLE IF EXISTS location;

CREATE TABLE flight (
    id SERIAL PRIMARY KEY,
    flight_number INTEGER NOT NULL,
    price INTEGER NOT NULL
);

CREATE TABLE location (
    id SERIAL PRIMARY KEY,
    name VARCHAR UNIQUE NOT NULL
);

CREATE TABLE room (
    id SERIAL PRIMARY KEY,
    location_id INTEGER NOT NULL REFERENCES location ( id ) ON DELETE CASCADE,
    price INTEGER NOT NULL
);

CREATE TABLE car (
    id SERIAL PRIMARY KEY,
    location_id INTEGER NOT NULL REFERENCES location ( id ) ON DELETE CASCADE,
    price INTEGER NOT NULL
);

CREATE TABLE customer (
    id SERIAL PRIMARY KEY
);

CREATE TABLE flight_reservation (
    flight_id INTEGER NOT NULL REFERENCES flight ( id ) ON DELETE CASCADE,
    customer_id INTEGER NOT NULL REFERENCES customer ( id ) ON DELETE CASCADE,
    CONSTRAINT flight_reservation_pk PRIMARY KEY ( flight_id, customer_id )
);

CREATE INDEX flight_reservation_flight_id_ix ON flight_reservation ( flight_id );
CREATE INDEX flight_reservation_customer_id_ix ON flight_reservation ( customer_id );

CREATE TABLE room_reservation (
    room_id INTEGER NOT NULL REFERENCES room ( id ) ON DELETE CASCADE,
    customer_id INTEGER NOT NULL REFERENCES customer ( id ) ON DELETE CASCADE,
    CONSTRAINT room_reservation_pk PRIMARY KEY ( room_id, customer_id )
);

CREATE INDEX room_reservation_room_id_ix ON room_reservation ( room_id );
CREATE INDEX room_reservation_customer_id_ix ON room_reservation ( customer_id );

CREATE TABLE car_reservation (
    car_id INTEGER NOT NULL REFERENCES car ( id ) ON DELETE CASCADE,
    customer_id INTEGER NOT NULL REFERENCES customer ( id ) ON DELETE CASCADE,
    CONSTRAINT car_reservation_pk PRIMARY KEY ( car_id, customer_id )
);

CREATE INDEX car_reservation_room_id_ix ON car_reservation ( car_id );
CREATE INDEX car_reservation_customer_id_ix ON car_reservation ( customer_id );
