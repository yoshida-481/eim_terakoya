set serveroutput on;

accept default_UserBoxIntegrationFlag number default 0 prompt 'Enter Default Value Of UserBoxIntegrationFlag 0 or 1 ( 0: not allow 1: allow ): '

declare 
	IS_NOT_MULTIPLE	constant	 number := 0;
	IS_MULTIPLE 	constant	 number := 1;
	
	rscode									number;
	
	attTypeName								varchar2(255);
	attTypeOtherName						varchar2(255);
	attTypeId_ManualDeletionProhibitedFlag	number;
	attTypeId_UserBoxIntegrationFlag		number;
	attTypeId_ThumbnailConversionDisabledFlag	number;
	attTypeId_PreviewConversionDisabledFlag		number;
	
	objTypeId_WorkspaceObjectType			number;
	objTypeId_UserObjectType				number;
	objTypeId_Document						number;
	
	cursor userObjectCursor is 
		select id 
		from EIMOBJ 
		where type 
		in 
		(
			select id 
			from EIMOBJTYPE 
			where name = 'ユーザ'
		);
	userRecord  userObjectCursor%ROWTYPE;
	
	type ref_cursor	is ref cursor;
	resultCursor						ref_cursor;
	
	appException						exception;
	

begin
	/* ========================================== */
	/* 属性タイプを作成する                       */
	/* ========================================== */
	
	attTypeName := '手動削除禁止フラグ';
	attTypeOtherName := 'Manual deletion prohibited Flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_ManualDeletionProhibitedFlag, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ManualDeletionProhibitedFlag, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ManualDeletionProhibitedFlag, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;

	attTypeName := 'ユーザ別Box連携利用許可フラグ';
	attTypeOtherName := 'User Box Integration Flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_UserBoxIntegrationFlag, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_UserBoxIntegrationFlag, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_UserBoxIntegrationFlag, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;

	attTypeName := 'サムネイル変換不可フラグ';
	attTypeOtherName := 'Thumbnail Conversion Disabled Flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_ThumbnailConversionDisabledFlag, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ThumbnailConversionDisabledFlag, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ThumbnailConversionDisabledFlag, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;

	attTypeName := 'プレビュー変換不可フラグ';
	attTypeOtherName := 'Preview Conversion Disabled Flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_PreviewConversionDisabledFlag, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PreviewConversionDisabledFlag, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PreviewConversionDisabledFlag, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;

	/* ========================================== */
	/* 属性タイプを割り当てる                     */
	/* ========================================== */
	select id into objTypeId_WorkspaceObjectType from EIMOBJTYPE where name = 'ワークスペース';
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkspaceObjectType, attTypeId_ManualDeletionProhibitedFlag, rscode);

	select id into objTypeId_UserObjectType from EIMOBJTYPE where name = 'ユーザ';
	ObjectAttributeUtils.applyAttributeType(objTypeId_UserObjectType, attTypeId_UserBoxIntegrationFlag, rscode);
	AttributeUtils.setDefaultValueInt(attTypeId_UserBoxIntegrationFlag, 0, &default_UserBoxIntegrationFlag, rscode);

	select id into objTypeId_Document from EIMOBJTYPE where name = 'ドキュメント';
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_ThumbnailConversionDisabledFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_PreviewConversionDisabledFlag, rscode);

	/* ==================================================================== */
	/* 既存のユーザにユーザ別Box連携利用許可フラグのデフォルト値を設定する  */
	/* ==================================================================== */
	open userObjectCursor;
		loop
			fetch userObjectCursor into userRecord;
				exit when userObjectCursor%notfound;
					ObjectAttributeUtils.setAttributeInteger(1, userRecord.id, attTypeId_UserBoxIntegrationFlag, 0, &default_UserBoxIntegrationFlag, 0, rscode);
		end loop;
	close userObjectCursor;

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
