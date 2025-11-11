--オブジェクトの移行状態を見るためのテーブル
--drop table IKOU_TARGET_WORKFLOW purge
--/

create table IKOU_TARGET_WORKFLOW
(
	workflow	number(32)		not null,
	ikou_flg	number(32)		not null,
	error		varchar2(2000),
	constraint PK_IKOU_TARGET_WORKFLOW primary key(workflow) using index
)
/

--drop table IKOU_TARGET_OBJ purge
--/

create table IKOU_TARGET_OBJ
(
	seq			number(32)		not null,
	id			number(32)		not null,
	workflow	number(32)		not null,
	ikou_flg	number(32)		not null,
	error		varchar2(2000),
	content01	varchar2(2000),
	content02	varchar2(2000),
	content03	varchar2(2000),
	content04	varchar2(2000),
	content05	varchar2(2000),
	content06	varchar2(2000),
	content07	varchar2(2000),
	content08	varchar2(2000),
	content09	varchar2(2000),
	content10	varchar2(2000),
	constraint PK_IKOU_TARGET_OBJ primary key(id) using index
)
/

create index IKOU_TARGET_OBJ_SEQ on IKOU_TARGET_OBJ(seq)
/

--drop table IKOU_OLD_EIMSTTYPE purge
--/
create table IKOU_OLD_EIMSTTYPE as select * from EIMSTTYPE where 1 = 2
/
alter table IKOU_OLD_EIMSTTYPE add STEP number(32)
/

DECLARE
	
	cursor c_eimsttype is
		select
			EST.workflow,
			EST.id,
			EST.name,
			LEVEL as step,
			EST.kind
		from EIMSTTYPE EST, EIMSTTYPEREL ESTR, EIMWF EW
		where EST.id = ESTR.child
			and EST.workflow = EW.id
		start with parent = 0
		connect by prior ESTR.child = ESTR.parent
		order by EST.workflow, step
	;
	
BEGIN
	
	/*----------------------------------------------------
	--旧ステータスタイプのバックアップを取得しておく(STEP付き)
	/*----------------------------------------------------*/
	FOR r_eimsttype IN c_eimsttype LOOP
		--auto列は使用しないので適当な値を入れておく
		insert into IKOU_OLD_EIMSTTYPE values(r_eimsttype.id, r_eimsttype.name, r_eimsttype.workflow, r_eimsttype.kind, -1,r_eimsttype.step);
	END LOOP;
	
	commit;
	
END;
/


