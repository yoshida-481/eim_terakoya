set serveroutput on
set trim on
set trims on
set lin 100
set pagesize 3000

col SEQ for 99999999999
col OBJECT_ID for 99999999999
col WORKFLOW_ID for 99999999999
col IKOU_STATUS for A40
col ERROR_CODE for 99999

COLUMN SPOOL_FILE_NAME NEW_VALUE SPOOL_FILE_NAME FORMAT A100
SELECT 'EIM_MIGRATION_' || TO_CHAR(SYSDATE,'YYYYMMDD_HH24MI') || '.log' SPOOL_FILE_NAME FROM DUAL;

SPOOL &SPOOL_FILE_NAME

TTITLE LEFT -
"■移行できなかったワークフローID"

select
	 workflow as WORKFLOW_ID
	,ikou_flg as ERROR_CODE
	,DECODE(ikou_flg, 0, '未処理', 'エラー') as IKOU_STATUS
from IKOU_TARGET_WORKFLOW
where ikou_flg <> 1
/

TTITLE LEFT -
"■移行できなかったオブジェクトID"

select
	 id as OBJECT_ID
	,workflow as WORKFLOW_ID
	,ikou_flg as ERROR_CODE
	,DECODE(ikou_flg, 0, '未処理', DECODE(ikou_flg, -1, 'エラー')) as IKOU_STATUS
from IKOU_TARGET_OBJ
where ikou_flg <> 1
/

SPOOL OFF

