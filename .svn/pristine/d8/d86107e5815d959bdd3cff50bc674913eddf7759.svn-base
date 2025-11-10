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
	objTypeId_WorkflowSetting			number;
	
	attTypeName							varchar2(255);
	attTypeOtherName					varchar2(255);
	attTypeId_ApproverCheckin			number;
	
	appException						exception;
	
begin
	
	/* ========================================== */
	/* 属性タイプを作成する               */
	/* ========================================== */
	attTypeName := 'チェックイン可能ステータス';
	attTypeOtherName := 'Status enable to checkin by approver';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_MULTIPLE, attTypeId_ApproverCheckin, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ApproverCheckin, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ApproverCheckin, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;
	
	
	/* ========================================== */
	/* オブジェクトタイプに属性タイプを割り当てる */
	/* ========================================== */
	select id into objTypeId_WorkflowSetting from EIMOBJTYPE where name = 'ワークフロー設定';
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowSetting, attTypeId_ApproverCheckin, rscode);
	
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
