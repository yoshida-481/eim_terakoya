/***************************************************/
/*                                                 */
/*  Create Update Notice Database Resources Script */
/*                                                 */
/***************************************************/

/***********/
/* Defines */
/***********/
accept EIM_TABLE_SPACE char default 'eim' prompt 'Enter EIMANAGER Table Space Name: '
accept EIM_INDEX_TABLE_SPACE char default 'eimidx' prompt 'Enter EIMANAGER Index Table Space Name: '

accept EIM_TABLE_SIZE char default '4M' prompt 'Enter EIMANAGER Table Space Size: '
accept EIM_INDEX_SIZE char default '16M' prompt 'Enter EIMANAGER Index Table Space Size: '

/*******************/
/* Create Sequence */
/*******************/
create sequence INDEXDATA_MGT_ID_SEQ increment by 1 start with 1 nomaxvalue nominvalue nocycle cache 20 noorder;
select INDEXDATA_MGT_ID_SEQ.nextval from dual;

/*****************/
/* Create Tables */
/*****************/
create table INDEXDATA_MGT
(
	key						number not null,
	id						varchar2(256) not null,
	data_kind				varchar2(256) not null,
	data_type				varchar2(256) not null,
	system_kind				varchar2(256) not null,
	update_kind				varchar2(32) not null,
	cdate					timestamp(6) default sysdate not null,
	constraint PK_INDEXDATA_MGT primary key(key) using index
		pctfree			5
		initrans		5
		maxtrans		255
		tablespace	&EIM_INDEX_TABLE_SPACE
		storage(initial &EIM_INDEX_SIZE)
) tablespace &EIM_TABLE_SPACE storage(initial &EIM_TABLE_SIZE);

