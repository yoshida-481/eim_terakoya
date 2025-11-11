
accept EIM_TABLE_SIZE char default '4M' prompt 'Enter EIMANAGER Table Space Size: '

create table EIMRELTMP
(
	id			number(32)	not null,
	type		number(32)	not null,
	parent		number(32)	not null,
	child		number(32)	not null
)storage(initial &EIM_TABLE_SIZE);

