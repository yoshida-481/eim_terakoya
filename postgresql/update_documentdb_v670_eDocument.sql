/* ======================================== */
/* 紙文書電子化オプション                        */
/*   追加設定スクリプト                          */
/* ======================================== */

SET client_encoding = 'UTF8';
\set ON_ERROR_STOP ON

DO $$
declare

	IS_NOT_MULTIPLE	constant	bigint := 0;
	IS_MULTIPLE 	constant	bigint := 1;

	rscode						bigint;

	objTypeId_Document			bigint;
	objTypeId_GroupObjectType	bigint;

	attTypeName					text;
	attTypeOtherName			text;

	attTypeId_CoverFlag			bigint;
	attTypeId_CoverCreationFlag	bigint;
	resultId					bigint;

begin

	/* ==================================== */
	/* 属性タイプを作成する                      */
	/* ==================================== */
	attTypeName := 'スキャン用表紙フラグ';
	attTypeOtherName := 'Cover flag for scanning';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_CoverFlag, rscode);
	if rscode <> 0 then
		raise exception using errcode = 'EIMEX', message = '「' || attTypeName || '」属性タイプを作成できませんでした。';
	end if;
	call othernameutils2.setothername(attTypeId_CoverFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	if rscode <> 0 then
		raise exception using errcode = 'EIMEX', message = '「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。';
	end if;
	call othernameutils2.setothername(attTypeId_CoverFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);
	if rscode <> 0 then
		raise exception using errcode = 'EIMEX', message = '「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。';
	end if;

	attTypeName := 'スキャン用表紙作成可否フラグ';
	attTypeOtherName := 'Cover creation permission flag for scanning';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_CoverCreationFlag, rscode);
	if rscode <> 0 then
		raise exception using errcode = 'EIMEX', message = '「' || attTypeName || '」属性タイプを作成できませんでした。';
	end if;
	call othernameutils2.setothername(attTypeId_CoverCreationFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	if rscode <> 0 then
		raise exception using errcode = 'EIMEX', message = '「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。';
	end if;
	call othernameutils2.setothername(attTypeId_CoverCreationFlag, '', null, 'EN', attTypeName, 4, resultId, rscode);
	if rscode <> 0 then
		raise exception using errcode = 'EIMEX', message = '「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。';
	end if;

	/* ==================================== */
	/* オブジェクトタイプに属性タイプを割り当てる         */
	/* ==================================== */

	/* ==================================== */
	/* Apply Attribute Type To Document     */
	/* ==================================== */
	select id into strict objTypeId_Document from EIMOBJTYPE where name = 'ドキュメント';
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_CoverFlag, null, rscode, resultId);

	/* ==================================== */
	/* Apply Attribute Type To Group Type   */
	/* ==================================== */
	select id into strict objTypeId_GroupObjectType from EIMOBJTYPE where name = 'グループ';
	call objecttypedao2.addattributetype(objTypeId_GroupObjectType, null, attTypeId_CoverCreationFlag, null, rscode, resultId);

exception
	when sqlstate 'EIMEX' then
		raise warning 'SQLSTATE = %', sqlstate;
		raise warning 'SQLERRM = %', sqlerrm;
		raise warning 'RSCODE = %', rscode;
		rollback;
		return;
	when OTHERS then
		raise warning 'SQLSTATE = %', sqlstate;
		raise warning 'SQLERRM = %', sqlerrm;
		rollback;
		return;

end
$$
;