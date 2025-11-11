set serveroutput on;

update eimobjtype set parent = null where name = 'フォルダ';

commit;
