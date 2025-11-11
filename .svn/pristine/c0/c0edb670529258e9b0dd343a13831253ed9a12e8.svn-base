set serveroutput on
DECLARE
	
	--**********
	--定数
	--**********
	CONS_ACRTYPE_ALWAYSREAD	constant number := 500;
	CONS_ACRTYPE_APPROVE	constant number := 101;
	
	/*----------------------------------------------------
	--system管理者を含む全てのアクセスエントリーをEIMACRから取得する
	--ステータス別セキュリティのレコードは除く
	/*----------------------------------------------------*/
	--0：拒否
	--1：許可
	--2：拒否
	
	cursor c_eimacr is
	select
		distinct id
	from
		eimacr
	where
		stsecid = 0
	order by id
	;
	
	--EIMACR
	type type_tab_eimacr is table of eimacr%rowtype index by binary_integer;
	tab_eimacr type_tab_eimacr;
	tab_eimacrread type_tab_eimacr;
	tab_eimacrnone type_tab_eimacr;
	eimacrIdx number := 0;
	eimacrreadIdx number := 0;
	eimacrnoneIdx number := 0;
	
	/*----------------------------------------------------
	--「読み取り」アクセス権限のみもっているEID(アクセスエントリー)を取得する
	/*----------------------------------------------------*/
	cursor c_acelist is
	with acrlist as
	(
		select
			 id
			,role
			,permit
		from
		eimacr
		where
		id in (
			select
			eid
			from
			eimace
			where
			sid in (
				select
					sid
				from
					eimsec
			)
		)
		and stsecid = 0
	)
	select
	X.id
	,X.cnt as Xcnt
	,Y.cnt as Ycnt
	,NVL(Z.cnt,0) as Zcnt
	from
	(
		--エントリーが持っているロールの数
		select id,count(id) as cnt from acrlist group by id
	) X
	,(
		--エントリーが持っている読み取りロールの数
		select id,count(id) as cnt from acrlist where role = 12 and permit = 1 group by id
	) Y
	,(
		--エントリーが持っている読み取りロール以外かつ読み取りロール以外のロールのアクセス権限が許可以外の数
		select id,count(id) as cnt from acrlist where role <> 12 and permit <> 1 group by id
	) Z
	where
	X.id = Y.id(+)
	and X.id = Z.id(+)
	and X.cnt = (Y.cnt + Z.cnt)
	;

	/*----------------------------------------------------
	--全てのアクセス権限が拒否or無視のEID(アクセスエントリー)を取得する
	/*----------------------------------------------------*/
	cursor c_acelist_none is
	with acrlist as
	(
		select
			 id
			,role
			,permit
		from
		eimacr
		where
		id in (
			select
			eid
			from
			eimace
			where
			sid in (
				select
					sid
				from
					eimsec
			)
		)
		and stsecid = 0
	)
	select
	X.id
	,X.cnt as Xcnt
	,Y.cnt as Ycnt
	from
	(
		--エントリーが持っているロールの数
		select id,count(id) as cnt from acrlist group by id
	) X
	,(
		--エントリーが持っているロールのうち許可以外(拒否or無視)の数
		select id,count(id) as cnt from acrlist where permit <> 1 group by id
	) Y
	where
	X.id = Y.id(+)
	and X.cnt = Y.cnt
	;
	
BEGIN

	/*----------------------------------------------------
	--「常時読取」アクセス権限を追加
	--(OTHERは共通で追加しているので本スクリプトでは本体のみ追加する)
	/*----------------------------------------------------*/
	insert into EIMACRTYPE values(CONS_ACRTYPE_ALWAYSREAD, 'ROLE_500');
	--insert into EIMACRTYPEOTHER values(CONS_ACRTYPE_ALWAYSREAD, 'JA', '常時読取');
	--insert into EIMACRTYPEOTHER values(CONS_ACRTYPE_ALWAYSREAD, 'EN', 'Always read');
	
	/*----------------------------------------------------
	--「読み取り」アクセス権限のみもっているEID(アクセスエントリー)を取得して
	--配列に格納する
	/*----------------------------------------------------*/
	FOR r_acelist IN c_acelist LOOP
		tab_eimacrread(eimacrreadIdx).id := r_acelist.id;
		eimacrreadIdx := eimacrreadIdx + 1;
	END LOOP;
	
	/*----------------------------------------------------
	--アクセス権限をもっていない(拒否or無視の)EID(アクセスエントリー)を取得して
	--配列に格納する
	/*----------------------------------------------------*/
	FOR r_acelist IN c_acelist_none LOOP
		tab_eimacrnone(eimacrnoneIdx).id := r_acelist.id;
		eimacrnoneIdx := eimacrnoneIdx + 1;
	END LOOP;
	
	/*----------------------------------------------------
	--EIMACRに全てのセキュリティ(SID)の常時読取アクセス権限を追加
	/*----------------------------------------------------*/
	tab_eimacr.delete;
	eimacrIdx := 0;
	FOR r_eimacr IN c_eimacr LOOP
		tab_eimacr(eimacrIdx).id := r_eimacr.id;
		eimacrIdx := eimacrIdx + 1;
	END LOOP;
	FOR I IN 0..tab_eimacr.count-1 LOOP
		--■取りあえず許可で追加
		--ASPは読取と書込の２種類のアプリケーション上の権限しかもたないため
		insert into EIMACR values(tab_eimacr(I).id, CONS_ACRTYPE_ALWAYSREAD, 0, 1);
		--■「承認」アクセス権限は拒否になっているのでとりあえず許可に更新する
		--insert into EIMACR values(tab_eimacr(I).id, CONS_ACRTYPE_APPROVE, 0, 1)
		update EIMACR set permit = 1 where permit = 0 and stsecid = 0 and role = CONS_ACRTYPE_APPROVE and id = tab_eimacr(I).id; 
	END LOOP;
	
	--1.「読み取り」アクセス権限しか持っていないアクセスエントリーの「常時読取」アクセス権限を拒否にする
	--2.「読み取り」アクセス権限しか持っていないアクセスエントリーの「承認」アクセス権限を拒否にする
	FOR I IN 0..tab_eimacrread.count-1 LOOP
		--DBMS_OUTPUT.PUT_LINE(I);
		--常時読取
		update EIMACR set permit = 0 where stsecid = 0 and role = CONS_ACRTYPE_ALWAYSREAD and id = tab_eimacrread(I).id;
		--承認
		update EIMACR set permit = 0 where stsecid = 0 and role = CONS_ACRTYPE_APPROVE and id = tab_eimacrread(I).id;
	END LOOP;
	
	--1.アクセス権限を持っていないアクセスエントリーの「常時読取」アクセス権限を拒否にする
	--2.アクセス権限を持っていないアクセスエントリーの「承認」アクセス権限を拒否にする
	FOR I IN 0..tab_eimacrnone.count-1 LOOP
		--DBMS_OUTPUT.PUT_LINE(I);
		--常時読取
		update EIMACR set permit = 0 where stsecid = 0 and role = CONS_ACRTYPE_ALWAYSREAD and id = tab_eimacrnone(I).id;
		--承認
		update EIMACR set permit = 0 where stsecid = 0 and role = CONS_ACRTYPE_APPROVE and id = tab_eimacrnone(I).id;
	END LOOP;
	
	commit;
	
END;
/


