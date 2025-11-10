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


set serveroutput on

declare 

	IS_NOT_MULTIPLE	constant	 number := 0;
	IS_MULTIPLE 	constant	 number := 1;
	
	rscode								number;
	found								number;
	
	objId								number;
	objName								varchar2(255);
	
	objTypeName							varchar2(255);
	objTypeOtherName					varchar2(255);
	objTypeId_ObjectType				number;
	objTypeId_OCR_Processing			number;
	objTypeId_Document					number;
	objTypeId_Workflow_Setting			number;
	
	attTypeName							varchar2(255);
	attTypeOtherName					varchar2(255);
	attTypeId_OCR_Status				number;
	attTypeId_OCR_Result_Status			number;
	attTypeId_OCR_Setting				number;
	
	appException						exception;
	
begin
	
	/* ========================================== */
	/* 属性タイプを作成する               */
	/* ========================================== */
	attTypeName := 'OCR処理ステータス';
	attTypeOtherName := 'OCR processing status';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_OCR_Status, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_OCR_Status, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_OCR_Status, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;
	
	attTypeName := 'OCR結果ステータス';
	attTypeOtherName := 'OCR result status';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_OCR_Result_Status, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_OCR_Result_Status, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_OCR_Result_Status, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;
	
	attTypeName := 'OCR設定有無';
	attTypeOtherName := 'OCR setting existence';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_OCR_Setting, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_OCR_Setting, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_OCR_Setting, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;
	
	
	select id into objTypeId_ObjectType from EIMOBJTYPE where name = 'オブジェクトタイプ';
	
	/* ========================================== */
	/* オブジェクトタイプを作成する               */
	/* ========================================== */
	objTypeName := 'OCR処理';
	objTypeOtherName := 'OCR processing';
	ObjectUtils.createObjectType(null, null, objTypeName, null, objTypeId_OCR_Processing, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_OCR_Processing, 'JA', objTypeName, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_OCR_Processing, 'EN', objTypeOtherName, rscode);
	objName := TO_CHAR(objTypeId_OCR_Processing);
	ObjectUtils.createObject(1, null, objTypeId_ObjectType, objName, 0, objId, rscode);
	
	/* ========================================== */
	/* オブジェクトタイプに属性タイプを割り当てる */
	/* ========================================== */
	select id into objTypeId_Document from EIMOBJTYPE where name = 'ドキュメント';
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_OCR_Status, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_OCR_Result_Status, rscode);
	
	select id into objTypeId_Workflow_Setting from EIMOBJTYPE where name = 'ワークフロー設定';
	ObjectAttributeUtils.applyAttributeType(objTypeId_Workflow_Setting, attTypeId_OCR_Setting, rscode);
	
	commit;
	
exception
	when appException then
		dbms_output.put_line('rscode=[ ' || rscode || ' ]');
		rollback;
	when OTHERS then
		dbms_output.put_line('エラー発生: ' || sqlerrm(sqlcode));
		rollback;

end;
/
