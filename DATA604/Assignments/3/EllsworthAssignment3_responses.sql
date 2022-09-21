-- DATA 604 Assignment 3
-- Michael Ellsworth
-- UCID30101253

SHOW DATABASES;
USE data604_a3;
SHOW TABLES;

DESCRIBE assessment;
DESCRIBE community;
DESCRIBE community_services;
DESCRIBE ward;
DESCRIBE wardcommunity;

-- Question 2 Part A
SELECT name, address
FROM community_services
WHERE type = 'Attraction';

-- Question 2 Part B
SELECT name
FROM community_services
WHERE type = 'Community Centre' AND address LIKE '%SW';

-- Question 2 Part C
SELECT name AS 'Community'
FROM community
WHERE sector = 'NORTHEAST' AND name LIKE '%ridge%';

-- Question 2 Part D
SELECT name AS 'Community'
FROM community
WHERE class = 'INDUSTRIAL';

-- Question 2 Part E
SELECT community.sector, community_services.name AS 'Service', community.comm_code
FROM community INNER JOIN community_services
ON community.comm_code = community_services.comm_code;

-- Question 2 Part F
SELECT DISTINCT wardcommunity.ward_number
FROM wardcommunity INNER JOIN assessment ON wardcommunity.comm_code = assessment.comm_code
WHERE assessment.median_value > 700000;

-- Question 2 Part G
SELECT community_services.name
FROM community INNER JOIN community_services ON community.comm_code = community_services.comm_code
WHERE community_services.type = 'Attraction' AND community.class <> 'Industrial' AND community.class <> 'Residential';

-- Question 2 Part H
SELECT community_services.name
FROM community_services INNER JOIN wardcommunity ON community_services.comm_code = wardcommunity.comm_code
WHERE community_services.type = 'PHS Clinic' AND wardcommunity.ward_number = 2;

-- Question 2 Part I
SELECT DISTINCT community.name AS 'Community Name', community_services.name AS 'Service'
FROM community LEFT JOIN community_services ON community.comm_code = community_services.comm_code;