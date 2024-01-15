DROP DATABASE IF EXISTS worlds;

CREATE DATABASE worlds;

DROP TABLE IF EXISTS world;

CREATE TABLE world(
	name varchar(30), 
	continent varchar(30), 
	area integer,
	population integer,
	gdp integer,
	primary key (name)
);

select * from world

ALTER TABLE world
ALTER COLUMN population TYPE bigint,
ALTER COLUMN gdp TYPE bigint;

INSERT INTO world(name, continent, area, population, gdp)
VALUES ('Afghanistan', 'Asia', 652230, 25500100, 20343000000),
       ('Albania', 'Europe', 28748, 2831741, 12960000000),
       ('Algeria', 'Africa', 2381741, 37100000, 188681000000),
       ('Andorra', 'Europe', 468, 78115, 3712000000),
       ('Angola', 'Africa', 1246700, 20609294, 100990000000);
	   
	   
SELECT name, area, population
from world
WHERE area >= 3000000 
or population >= 25000000;

-- problem 2
DROP type IF EXISTS fats;
DROP type IF EXISTS rec;

create type fats as enum('Y', 'N');
create type rec as enum('Y', 'N');

DROP TABLE  IF EXISTS products;

create table products(
	product_id int,
	low_fats fats,
	recyclable rec,
	primary key (product_id)
);

select * from products

insert into products(product_id, low_fats, recyclable)
values (0,'Y','N'),
		(1,'Y','Y'),
		(2,'N','Y'),
		(3,'Y','Y'),
		(4,'N','N');


select product_id
from products
where low_fats = 'Y'
AND recyclable = 'Y';


