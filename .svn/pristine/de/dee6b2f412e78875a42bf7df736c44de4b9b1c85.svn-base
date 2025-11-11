set serveroutput on

declare 

	IS_NOT_MULTIPLE	constant	 number := 0;
	IS_MULTIPLE 	constant	 number := 1;
	
	rscode								number;
	found								number;
	relTypeName							varchar2(255);
	attTypeName							varchar2(255);
	attTypeOtherName					varchar2(255);
	
	attTypeId_linkUpdateTiming			number;
	relTypeId_Link						number;
	linkUpdateTimingValue				number := 0;
	
	appException						exception;
	
	commitIdx							number := 0;
	commitCnt				constant	number := 1000;
	
begin
	
	-- リンク更新タイミングは 0:手動 固定
	linkUpdateTimingValue := 0;
	
	-- Create Attribute Type
	attTypeName := 'リンク更新タイミング';
	attTypeOtherName := 'Update Timing Linked';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_linkUpdateTiming, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「リンク更新タイミング」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_linkUpdateTiming, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「リンク更新タイミング」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_linkUpdateTiming, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「リンク更新タイミング」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;
	
	-- Get Relation Type Id 'Link'
	relTypeName := 'リンク';
	select count(id) into found from EIMRELTYPE where name = relTypeName;
	if found = 0 then
		dbms_output.put_line('「リンク」リレーションタイプを取得できませんでした。');
		rscode := -1;
		raise appException;
	end if;
	select id into relTypeId_Link from EIMRELTYPE where name = relTypeName;

	-- Apply Attribute Type to 'Link'
	RelationAttributeUtils.applyAttributeType(relTypeId_Link, attTypeId_linkUpdateTiming, rscode);
	
	-- Search 'Link' Relation 
	declare
		cursor cursor_relIds is select id from EIMREL where type = relTypeId_Link;
	begin
		for cursor_relId in cursor_relIds loop
			RelationAttributeUtils.setAttributeInteger(null, cursor_relId.id, attTypeId_linkUpdateTiming, 0, linkUpdateTimingValue, 0, rscode);
			commitIdx := commitIdx + 1;
			-- commitCntの件数に達したらcommit
			IF commitIdx = commitCnt THEN
				dbms_output.put_line(commitIdx);
				commit;
				commitIdx := 0;
			END IF;
		end loop;
	end;
	
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
