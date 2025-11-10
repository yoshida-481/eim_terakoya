CREATE OR REPLACE PACKAGE ReplaceUtils
as

	type ref_cursor is ref cursor;

	/* Result Code */
	NO_ERROR												number	:= 0;
	UNEXPECTED_ERROR										number	:= -1;


procedure replaceAllPaths(
	I_objectId			in	number,
	I_objNameOld		in	varchar2,
	I_objNameNew		in	varchar2,
	I_attrTypeNamePath	in	varchar2,
	I_relTypeNamePath	in	varchar2,
	I_typeNumber		in	number,
	O_rscode			out	number);

end ReplaceUtils;

/
