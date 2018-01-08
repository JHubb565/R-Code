-- This SQL creates the ListOfOrders table
CREATE TABLE listoforders(
OrderID	      char(15), 
OrderDate     date,
CustomerName  char(50),
City          char(50),	
Country       char(50),
Region        char(50),	
Segment	      char(50),
ShipDate	  date,
ShipMode	  char(50),
State         char(50)
);

-- This SQL creates the ListOfOrders table
COPY listoforders FROM 'C:/Users/malan/Dropbox/_Purdue/_Teaching/DM/3_Relational DBs & Postgres/data/ListOfOrders.csv' CSV HEADER DELIMITER ',' encoding 'windows-1251';

-- This SQL creates the ListOfOrders table
COPY listoforders FROM 'C:/Users/malan/Desktop/ListOfOrders.csv' CSV HEADER DELIMITER ',' encoding 'windows-1251';
-- For MAC folks
COPY listoforders FROM 'C:/Users/malan/Desktop/ListOfOrders.csv' CSV HEADER DELIMITER ',';

SELECT *
FROM listoforders
WHERE Region = 'North'

SELECT *
FROM listoforders
WHERE Region = 'North'
AND orderdate between '2011-01-01' and '2011-02-01'
ORDER BY orderdate ASC

SELECT *
FROM listoforders
WHERE Region = 'North'
AND orderdate >= '2014-12-30'
ORDER BY shipdate DESC

SELECT DISTINCT segment, region
FROM listoforders
ORDER BY segment, region

SELECT region, count(city)
FROM listoforders
GROUP BY region

SELECT country, shipmode, count(shipmode) as CntShipMode
FROM listoforders
WHERE country IN ('France','Germany')
GROUP BY country, shipmode
ORDER BY country ASC, count(shipmode) DESC


SELECT a.shipmode, sum(a.cntshipmode)
FROM(
    SELECT country, shipmode, count(shipmode) as CntShipMode
    FROM listoforders
    WHERE country IN ('France','Germany')
    GROUP BY country, shipmode
    ORDER BY country ASC, count(shipmode) DESC
    ) a
GROUP BY a.shipmode

select customername, count(orderid)
from listoforders
GROUP BY customername
HAVING count(orderid) >= 11
limit 1


