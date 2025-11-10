SET client_encoding = 'UTF8';
\set ON_ERROR_STOP ON

\prompt 'Enter Default Value Of UserBoxIntegrationFlag ( 0: not allow 1: allow ): ' default_UserBoxIntegrationFlag
\o /dev/null
select set_config('psql.default_UserBoxIntegrationFlag', :'default_UserBoxIntegrationFlag', false);
\o

DO $$
declare

	IS_NOT_MULTIPLE	constant				bigint := 0;
	IS_MULTIPLE 	constant				bigint := 1;

	rscode									bigint;

	attTypeName								text;
	attTypeOtherName						text;
	attTypeId_UserBoxIntegrationFlag		bigint;
	attTypeId_ManualDeletionProhibitedFlag	bigint;
	attTypeId_ThumbnailConversionDisabledFlag	bigint;
	attTypeId_PreviewConversionDisabledFlag		bigint;

	objTypeId_Workspace						bigint;
	objTypeId_UserObjectType				bigint;
	objTypeId_Document						bigint;
	default_UserBoxIntegrationFlag			bigint;

	userObjectCursor  CURSOR FOR
		select id 
		from EIMOBJ 
		where type 
		in 
		(
			select id 
			from EIMOBJTYPE 
			where name = 'ユーザ'
		);
	userRecord  						RECORD;
	resultCursor						refcursor;
	resultId							bigint;
	errorValue							bigint;

begin

	/* ==================================== */
	/* 属性タイプを作成する                      */
	/* ==================================== */
	attTypeName := '手動削除禁止フラグ';
	attTypeOtherName := 'Manual deletion prohibited Flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_ManualDeletionProhibitedFlag, rscode);
	if rscode <> 0 then
		raise exception using errcode = 'EIMEX', message = '「' || attTypeName || '」属性タイプを作成できませんでした。';
	end if;
	call othernameutils2.setothername(attTypeId_ManualDeletionProhibitedFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	if rscode <> 0 then
		raise exception using errcode = 'EIMEX', message = '「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。';
	end if;
	call othernameutils2.setothername(attTypeId_ManualDeletionProhibitedFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);
	if rscode <> 0 then
		raise exception using errcode = 'EIMEX', message = '「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。';
	end if;

	attTypeName := 'ユーザ別Box連携利用許可フラグ';
	attTypeOtherName := 'User Box Integration Flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, null, attTypeId_UserBoxIntegrationFlag, rscode);
	if rscode <> 0 then
		raise exception using errcode = 'EIMEX', message = '「' || attTypeName || '」属性タイプを作成できませんでした。';
	end if;
	call othernameutils2.setothername(attTypeId_UserBoxIntegrationFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	if rscode <> 0 then
		raise exception using errcode = 'EIMEX', message = '「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。';
	end if;
	call othernameutils2.setothername(attTypeId_UserBoxIntegrationFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);
	if rscode <> 0 then
		raise exception using errcode = 'EIMEX', message = '「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。';
	end if;

	attTypeName := 'サムネイル変換不可フラグ';
	attTypeOtherName := 'Thumbnail Conversion Disabled Flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, null, attTypeId_ThumbnailConversionDisabledFlag, rscode);
	if rscode <> 0 then
		raise exception using errcode = 'EIMEX', message = '「' || attTypeName || '」属性タイプを作成できませんでした。';
	end if;
	call othernameutils2.setothername(attTypeId_ThumbnailConversionDisabledFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	if rscode <> 0 then
		raise exception using errcode = 'EIMEX', message = '「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。';
	end if;
	call othernameutils2.setothername(attTypeId_ThumbnailConversionDisabledFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);
	if rscode <> 0 then
		raise exception using errcode = 'EIMEX', message = '「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。';
	end if;

	attTypeName := 'プレビュー変換不可フラグ';
	attTypeOtherName := 'Preview Conversion Disabled Flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, null, attTypeId_PreviewConversionDisabledFlag, rscode);
	if rscode <> 0 then
		raise exception using errcode = 'EIMEX', message = '「' || attTypeName || '」属性タイプを作成できませんでした。';
	end if;
	call othernameutils2.setothername(attTypeId_PreviewConversionDisabledFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	if rscode <> 0 then
		raise exception using errcode = 'EIMEX', message = '「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。';
	end if;
	call othernameutils2.setothername(attTypeId_PreviewConversionDisabledFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);
	if rscode <> 0 then
		raise exception using errcode = 'EIMEX', message = '「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。';
	end if;

	/* ================================================== */
	/* オブジェクトタイプに属性タイプを割り当てる         */
	/* ================================================== */

	/* ===================================== */
	/* Apply Attribute Type To WorkSpace     */
	/* ===================================== */
	select id into strict objTypeId_Workspace from EIMOBJTYPE where name = 'ワークスペース';
	call objecttypedao2.addattributetype(objTypeId_Workspace, null, attTypeId_ManualDeletionProhibitedFlag, null, rscode, resultId);

	select id into strict objTypeId_UserObjectType from EIMOBJTYPE where name = 'ユーザ';
	call objecttypedao2.addattributetype(objTypeId_UserObjectType, null, attTypeId_UserBoxIntegrationFlag, null, rscode, resultId);
	select current_setting('psql.default_UserBoxIntegrationFlag',false) into strict default_UserBoxIntegrationFlag;
	call attributetypedao2.setdefaultvaluelong(attTypeId_UserBoxIntegrationFlag, null, 0, default_UserBoxIntegrationFlag, 0, rscode, resultId);

	select id into strict objTypeId_Document from EIMOBJTYPE where name = 'ドキュメント';
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_ThumbnailConversionDisabledFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_PreviewConversionDisabledFlag, null, rscode, resultId);

	/* ==================================================================== */
	/* 既存のユーザにユーザ別Box連携利用許可フラグのデフォルト値を設定する  */
	/* ==================================================================== */
	open userObjectCursor;
		loop
			fetch userObjectCursor into userRecord;
				exit when not found;
					call AttributeUtils2.setAttributelong(1, cast(userRecord.id as bigint), attTypeId_UserBoxIntegrationFlag, null, 0, default_UserBoxIntegrationFlag, 1, 1, 0, resultCursor, rscode, errorValue);
		end loop;
	close userObjectCursor;

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
