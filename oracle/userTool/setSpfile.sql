set serveroutput on

set define on

define DIR_PATH = '/home/oracle/eim/userimp';

-- 初期設定を確認
SELECT NAME,VALUE FROM V$PARAMETER2 WHERE NAME='utl_file_dir';

-- 設定を変更
ALTER　SYSTEM　SET　UTL_FILE_DIR='&DIR_PATH'　SCOPE=MEMORY;

-- 変更内容を確認
SELECT NAME,VALUE FROM V$PARAMETER2 WHERE NAME='utl_file_dir';