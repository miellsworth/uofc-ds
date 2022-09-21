SELECT COUNT(*) FROM rainchannel_measurements;
SELECT channel, sum(rainfall_mm) AS channel_total
FROM rainchannel_measurements 
WHERE measurement_year = 2012
GROUP BY channel
ORDER BY channel_total;

SELECT * FROM
		(SELECT channel, SUM(rainfall_mm) AS
        channel_sum from rainchannel_measurements
        where measurement_year = 2012
        GROUP BY channel) AS channel_totals
        WHERE channel_sum < 250
ORDER BY channel_sum;

SELECT DISTINCT channel FROM rainchannel_measurements
WHERE rainfall_mm > 8;

SELECT DISTINCT channel FROM rainchannel_measurements
WHERE channel not in
	(SELECT DISTINCT channel FROM rainchannel_measurements
    WHERE rainfall_mm > 8);

SELECT channel, sum(rainfall_mm), month(measurement_ts), year(measurement_ts)
FROM rainchannel_measurements
WHERE month(measurement_ts) = 6 OR month(measurement_ts) = 7
GROUP BY year(measurement_ts), channel, month(measurement_ts);

SELECT channel, sum(rainfall_mm), month(measurement_ts), year(measurement_ts)
FROM rainchannel_measurements
WHERE month(measurement_ts) = 6
GROUP BY year(measurement_ts), channel, month(measurement_ts);

SELECT channel, sum(rainfall_mm), month(measurement_ts), year(measurement_ts)
FROM rainchannel_measurements
WHERE month(measurement_ts) = 7
GROUP BY year(measurement_ts), channel, month(measurement_ts);