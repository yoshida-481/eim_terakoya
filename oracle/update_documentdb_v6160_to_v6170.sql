set serveroutput on;

drop table EIMPAGE cascade constraint purge;
drop table EIMDOCKEYWORD cascade constraint purge;

drop package DocKeywordDao;
drop package DocResource;

