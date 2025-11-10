set serveroutput on size 100000
DECLARE
	--**********
	--定数
	--**********
	CONS_ACRTYPE_READ		constant number := 12;
	CONS_ACRPERMIT_KYOHI	constant number := 0;
	
	--**********
	--カーソル
	--**********
	--移行対象のワークフローの公開済みを除いたステータスタイプ
	cursor c_eimsttype is
		select
		*
		from
		eimsttype
		where
		workflow in
		(
			select
			workflow
			from
			ikou_target_workflow
			where
			ikou_flg = 1
		)
		and kind <> -13004
		order by workflow, id
	;
	
	--セキュリティ一覧
	cursor c_eimsec is
		select
			 id
			,name
		from
			eimsec
		order by id
	;
	
	--ステータス別セキュリティを取得する
	cursor c_eimstsec is
		select
			*
		from
			eimstsec
		order by id
		;
	
	--読み取り権限のみもっているエントリー
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

BEGIN
	
	--セキュリティの数だけEIMSTSECを作成する
	FOR r_eimsec IN c_eimsec LOOP
		--移行対象ワークフローの公開済みステータスタイプを除いたステータスタイプの数だけEIMSTSECを作成する
		FOR r_eimsttype IN c_eimsttype LOOP
			insert into EIMSTSEC values(EIMID.nextval, r_eimsec.id, r_eimsttype.id);
		END LOOP;
	END LOOP;
	
	--読み取り権限だけもっているアクセスエントリーのステータス別セキュリティのEIMACRを作成する
	FOR r_acelist IN c_acelist LOOP
		FOR r_eimstsec IN c_eimstsec LOOP
			insert into EIMACR values(r_acelist.id, CONS_ACRTYPE_READ, r_eimstsec.id, CONS_ACRPERMIT_KYOHI);
		END LOOP;
	END LOOP;
	
	commit;
	
END;
/

