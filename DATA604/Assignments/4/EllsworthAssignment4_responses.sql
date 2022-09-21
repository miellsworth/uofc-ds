-- DATA 604 Assignment 4
-- Michael Ellsworth
-- UCID30101253

SHOW DATABASES;
USE movies;
SHOW TABLES;

DESCRIBE cast;
DESCRIBE collection;
DESCRIBE crew;
DESCRIBE genre;
DESCRIBE movie;
DESCRIBE movie_genre;
DESCRIBE person;
DESCRIBE production_company;
DESCRIBE production_country;

-- Part 1
-- 1
SELECT title,
(revenue - budget) as profit, vote_average
FROM movie
WHERE (revenue - budget) < 0
AND vote_average > 8
ORDER BY profit;

-- 2
SELECT production_company.name
FROM production_country
INNER JOIN production_company
USING (tmdb_id)
WHERE production_country.name LIKE 'Liechtenstein';

-- 3
SELECT person.name
FROM person
INNER JOIN (
	SELECT *
	FROM cast
	INNER JOIN movie
	USING (tmdb_id)
	WHERE movie.title LIKE 'Cast Away') as cast
USING (person_id);

-- Part 2
-- 4
SELECT COUNT(title)
FROM movie
WHERE budget > 100000000;

-- 5
SELECT COUNT(DISTINCT production_country.name)
FROM production_country
INNER JOIN movie
USING (tmdb_id);

-- 6
SELECT COUNT(movie.title) AS count_movie, collection.name
FROM collection
INNER JOIN movie
USING (collection_id)
GROUP BY collection.collection_id
ORDER BY count_movie DESC
LIMIT 5;

-- 7
SELECT AVG(movie.revenue) AS avg_revenue, production_country.name
FROM movie
INNER JOIN production_country
USING (tmdb_id)
GROUP BY production_country.name
ORDER BY avg_revenue DESC;

-- 8
SELECT COUNT(movie.title) movie_count, production_company.name
FROM movie
INNER JOIN production_company
USING (tmdb_id)
GROUP BY production_company.name
ORDER BY movie_count DESC
LIMIT 1;

-- 9
SELECT movie_genres.name, count(movie_genres.name), AVG(movie_genres.revenue)
FROM (
	SELECT *
    FROM movie
    INNER JOIN (
		SELECT *
        FROM genre
        INNER JOIN movie_genre
        USING (genre_id)) genre_names
    USING (tmdb_id)) AS movie_genres
INNER JOIN (
	SELECT *
	FROM cast
	INNER JOIN person
	USING (person_id)) AS cast_names
USING(tmdb_id)
WHERE cast_names.name LIKE 'Tom Hanks'
GROUP BY movie_genres.name;

-- Part 3
-- 10
SELECT SUM(revenue) AS sum_revenue, collection.name
FROM movie
INNER JOIN collection
USING (collection_id)
GROUP BY collection.name
ORDER BY sum_revenue DESC
LIMIT 3;

-- 11
SELECT MAX(canada.budget) as max_budget, AVG(canada.budget) avg_budget, genre_names.name as genre
FROM (
	SELECT * 
	FROM movie_genre
	INNER JOIN genre
	USING (genre_id)) genre_names
INNER JOIN (
    SELECT *
    FROM movie
    INNER JOIN production_country
    USING (tmdb_id)
    WHERE production_country.name LIKE 'CANADA') as canada
USING (tmdb_id)
GROUP BY genre_names.name;

-- Part 4
-- 12
SELECT DISTINCT cast_names.name
FROM (
	SELECT DISTINCT movie.tmdb_id, movie.title
	FROM movie
	INNER JOIN (
		SELECT *
		FROM cast
		INNER JOIN person
		USING (person_id)) as cast_names
	USING (tmdb_id)
	WHERE cast_names.name LIKE 'Kevin Bacon') as kevin_bacon_movies
INNER JOIN (
	SELECT *
	FROM cast
	INNER JOIN person
	USING (person_id)) as cast_names
USING (tmdb_id)
WHERE cast_names.name != 'Kevin Bacon';

-- 13
SELECT COUNT(DISTINCT cast_names.name)
FROM (
	SELECT DISTINCT movie.title, movie.tmdb_id
	FROM movie
	INNER JOIN(
		SELECT *
		FROM cast
		INNER JOIN person
		USING (person_id)) as cast_names
	USING (tmdb_id)
	WHERE cast_names.name IN (
		SELECT DISTINCT cast_names.name
		FROM (
			SELECT DISTINCT movie.tmdb_id, movie.title
			FROM movie
			INNER JOIN (
				SELECT *
				FROM cast
				INNER JOIN person
				USING (person_id)) as cast_names
			USING (tmdb_id)
			WHERE cast_names.name LIKE 'Kevin Bacon') AS kevin_bacon_movies
		INNER JOIN (
			SELECT *
			FROM cast
			INNER JOIN person
			USING (person_id)) as cast_names
		USING (tmdb_id)
		WHERE cast_names.name != 'Kevin Bacon')) AS bacon_tables
INNER JOIN (
	SELECT *
	FROM cast
	INNER JOIN person
	USING (person_id)) as cast_names
USING (tmdb_id)
WHERE cast_names.name != 'Kevin Bacon';

-- 14
-- I do not believe this question is solvable using the methods
-- described in DATA 604 without timing out the SQL query. However,
-- based on a quick Google search of "Six Degrees of Kevin Bacon",
-- it appears that every person in the cast table is related to
-- Kevin Bacon within six degrees. Therefore, the following query should
-- output the same result as the query that was intended for this question
SELECT COUNT(DISTINCT cast.person_id) - 1
FROM cast;

-- 15
SELECT COUNT(DISTINCT cast.person_id)
FROM cast;