/****************************************************/
/*                                                  */
/*  Create EIMANAGER Form Database Resources Script */
/*                                                  */
/****************************************************/

set serveroutput on;

accept EIMDOC_TEXT_INDEX_TABLE_SPACE char default 'eimidx' prompt 'Enter EIMANAGER Text Index Table Space Name: '

/***************************************/
/* Document Keyword Search For Oracle Text */
/***************************************/
create table EIMDOCKEYWORD
(
	id		number(32)	not null,
	type	number(32)	not null,
	value	CLOB		not null
);

/* For Oracle Text */
exec ctx_ddl.create_preference('EIMDOCKEYWORD_LEXER',   'JAPANESE_VGRAM_LEXER');
exec ctx_ddl.create_preference('EIMDOCKEYWORD_STORAGE','BASIC_STORAGE');
exec ctx_ddl.set_attribute('EIMDOCKEYWORD_STORAGE', 'I_TABLE_CLAUSE', 'tablespace ' || '&EIMDOC_TEXT_INDEX_TABLE_SPACE' );
exec ctx_ddl.set_attribute('EIMDOCKEYWORD_STORAGE', 'K_TABLE_CLAUSE', 'tablespace ' || '&EIMDOC_TEXT_INDEX_TABLE_SPACE' );
exec ctx_ddl.set_attribute('EIMDOCKEYWORD_STORAGE', 'R_TABLE_CLAUSE', 'tablespace ' || '&EIMDOC_TEXT_INDEX_TABLE_SPACE' );
exec ctx_ddl.set_attribute('EIMDOCKEYWORD_STORAGE', 'N_TABLE_CLAUSE', 'tablespace ' || '&EIMDOC_TEXT_INDEX_TABLE_SPACE' );
exec ctx_ddl.set_attribute('EIMDOCKEYWORD_STORAGE', 'I_INDEX_CLAUSE', 'tablespace ' || '&EIMDOC_TEXT_INDEX_TABLE_SPACE' );

create index EIMDOCKEYWORDIDX
on EIMDOCKEYWORD(value)
indextype is ctxsys.context
parameters('STORAGE EIMDOCKEYWORD_STORAGE LEXER EIMDOCKEYWORD_LEXER');

/************/
/* Packages */
/************/
@@update_doc_packages.sql

commit;
quit
