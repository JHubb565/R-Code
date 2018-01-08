-- This SQL creates the OrderBreakdown table
CREATE TABLE orderbreakdown(
OrderID	     char(15), 
ProductName  char(70), 	
Discount     numeric,	
Sales	     numeric,
Profit	     numeric,
Quantity     numeric,
Category     char(15),  	
SubCategory  char(15)
);

-- This SQL creates the ListOfOrders table
COPY orderbreakdown FROM 'C:/Users/malan/Dropbox/_Purdue/_Teaching/DM/3_Relational DBs & Postgres/data/Orderbreakdown.dlm' CSV HEADER DELIMITER '|' encoding 'windows-1251';

--inner join example
select *
from listoforders l
inner join orderbreakdown o on l.orderid = o.orderid
order by l.orderid

--how many records were returned from the inner join
select count(a.*)
from(
    select *
    from listoforders l
    inner join orderbreakdown o on l.orderid = o.orderid
    order by l.orderid
	) a;
    
--8047 is the same number of records in orderbreakdown
select count(*)
from orderbreakdown

--left join example
select *
from listoforders l
left join orderbreakdown o on l.orderid = o.orderid
order by l.orderid

--right join example
select *
from listoforders l
right join orderbreakdown o on l.orderid = o.orderid
order by l.orderid

--outer join example
select *
from listoforders l
full outer join orderbreakdown o on l.orderid = o.orderid
order by l.orderid

--all info from ordersbreakdown plus additional features I want
select o.*, l.orderdate, l.customername, l.city, l.country, l.region, l.segment,
l.shipdate, l.shipmode, l.state
from orderbreakdown o
inner join listoforders l on o.orderid = l.orderid
order by o.orderid

--summarize discount, sales, profit, qty per customer
select l.customername, count(o.orderid), avg(discount), sum(sales), sum(profit), sum(quantity)
from orderbreakdown o
inner join listoforders l on o.orderid = l.orderid
group by l.customername
order by l.customername

--summarize discount, sales, profit, qty per customer (better version)
select l.customername, count(o.orderid) as NumOrders, round(avg(discount),3) as AvgDiscount
, sum(sales) as SumSales, sum(profit) as SumProfit, sum(quantity) as SumQty
from orderbreakdown o
inner join listoforders l on o.orderid = l.orderid
group by l.customername
order by l.customername

--Create a table that summarizes discount, sales, profit, qty per customer
CREATE TABLE customerstats AS
select l.customername, count(o.orderid) as NumOrders, round(avg(discount),3) as AvgDiscount
, sum(sales) as SumSales, sum(profit) as SumProfit, sum(quantity) as SumQty
from orderbreakdown o
inner join listoforders l on o.orderid = l.orderid
group by l.customername
order by l.customername