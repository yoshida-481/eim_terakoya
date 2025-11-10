/***************************************************/
/*                                                 */
/*  Create Update Notice Database Resources Script */
/*    for PostgreSQL                               */
/*                                                 */
/***************************************************/

/***********/
/* Defines */
/***********/
\prompt 'Enter EIMANAGER Index Table Space Name: ' EIM_INDEX_TABLE_SPACE

/*****************/
/* Create Tables */
/*****************/

CREATE TABLE indexdata_mgt (
    key bigint NOT NULL,
    id character varying(256) NOT NULL,
    data_kind character varying(256) NOT NULL,
    data_type character varying(256) NOT NULL,
    system_kind character varying(256) NOT NULL,
    update_kind character varying(32) NOT NULL,
    cdate timestamp without time zone DEFAULT statement_timestamp() NOT NULL
);



ALTER TABLE ONLY indexdata_mgt
    ADD CONSTRAINT indexdata_mgt_pkey PRIMARY KEY (key) USING INDEX TABLESPACE :EIM_INDEX_TABLE_SPACE;


