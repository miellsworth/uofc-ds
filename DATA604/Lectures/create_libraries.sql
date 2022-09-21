drop table library_locations;

create table library_locations (
	 Library varchar(40) NOT NULL,
	 Postal_Code varchar(7),
	 Square_Feet int, 
	 Phone_Number varchar(12),
	 Monday_Open time,
	 Monday_Close time,
	 Tuesday_Open time,
	 Tuesday_Close time,
	 Wednesday_Open time,
	 Wednesday_Close time,
	 Thursday_Open time,
	 Thursday_Close time,
	 Friday_Open time,
	 Friday_Close time,
	 Saturday_Open time,
	 Saturday_Close time,
	 Sunday_Open time,
	 Sunday_Close time,
	 Address  varchar(100)
	);
	
