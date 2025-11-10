create or replace package body ReplaceUtils
as


/* Check Attribute Type */
procedure replaceAllPaths(
	I_objectId			in	number,
	I_objNameOld		in	varchar2,
	I_objNameNew		in	varchar2,
	I_attrTypeNamePath	in	varchar2,
	I_relTypeNamePath	in	varchar2,
	I_typeNumber		in	number,
	O_rscode			out	number)
is
	found				number;
	attrTypeId			number;
	relTypeId			number;
	orgPath				varchar2(512);
	re_orgPath  		varchar2(512);
	srcPath				varchar2(512);
	destPath			varchar2(512);
	objVersionCursor			ref_cursor;
	i			number;
	
	TYPE IDVALUE_RECTYPE IS RECORD(
		OBJID	number,
		VALUE	varchar2(512)
	);
		
	TYPE VALUE_TYPE IS TABLE OF IDVALUE_RECTYPE;
	VALUE1	VALUE_TYPE;
	
begin

	/* Check Exists Object */
	select count(id) into found from EIMOBJ where id = I_objectId;
	if found = 0 then
		O_rscode := EIMResource.ERR_OBJECT_NOTFOUND;
		return;
	end if;
	
	/* get Path AttributeType */
	select id into attrTypeId from EIMATTR where name = I_attrTypeNamePath;
	
	/* get Path RelationType */
	select id into relTypeId from EIMRELTYPE where name = I_relTypeNamePath;
	
	/* folder=0, workspace=1 */
	if I_typeNumber = 0  then
	
		/* get OrgObject path */
		select value into orgPath from EIMOBJSTR where type = attrTypeId and key = 1 and id = I_objectId;
		
		    /* re_orgpath replace*/
		re_orgpath := REPLACE(orgpath, '(','\(');
		re_orgpath := REPLACE(re_orgpath, ')','\)');
		re_orgpath := REPLACE(re_orgpath, '[','\[');
		re_orgpath := REPLACE(re_orgpath, ']','\]');
		re_orgpath := REPLACE(re_orgpath, '+','\+');
		re_orgpath := REPLACE(re_orgpath, '^','\^');
		re_orgpath := REPLACE(re_orgpath, '$','\$');
		re_orgpath := REPLACE(re_orgpath, '.','\.');

		/* path replace */
		srcPath := '^' || re_orgpath || I_objNameOld || '/';
		destPath := orgpath || I_objNameNew || '/';
		
	else
	
		/* path replace */
		srcPath := '^' || '/' || I_objNameOld || '/';
		destPath := '/' || I_objNameNew || '/';
	
	end if;
	
	
	/* get All Object Id */
	declare 
		CURSOR objVersionCursor is 
			select
				EO.id as OBJID,ES.value as value
			from
				EIMVER EV, EIMOBJ EO, (select * from EIMOBJSTR where type = attrTypeId and key = 1)ES
			where
				EV.vid in 
				(
					select vid from EIMVER where oid in 
					(
						SELECT child FROM EIMREL 
						where type = relTypeId
						start with parent = I_objectId connect by prior child = parent
					)
				)
				and EV.oid = EO.id
				and EO.id = ES.id
			order by
				EO.id;
	begin
		open objVersionCursor;
		LOOP
			fetch objVersionCursor BULK COLLECT INTO VALUE1;
			EXIT WHEN VALUE1.COUNT = 0;
			
			BEGIN
				FORALL i IN 1..VALUE1.COUNT
					delete from EIMOBJSTR EOS where EOS.type = attrTypeId and EOS.id = VALUE1(i).OBJID;
				FORALL i IN 1..VALUE1.COUNT
					insert into EIMOBJSTR EOS values(VALUE1(i).OBJID, attrTypeId, 1, REGEXP_REPLACE(VALUE1(i).VALUE,srcPath,destPath,1,1));
			END;
		END LOOP;
		close objVersionCursor;
		
	end;
	
	O_rscode := EIMResource.NO_ERROR;
	
exception
	when others then
		O_rscode := EIMResource.UNEXPECTED_ERROR;

end replaceAllPaths;


end ReplaceUtils;
/
