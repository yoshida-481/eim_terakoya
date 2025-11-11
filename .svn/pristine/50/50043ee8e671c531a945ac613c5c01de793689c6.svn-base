set serveroutput on size 100000
DECLARE
	
	--**********
	--定数
	--**********
	
	--**********
	--変数
	--**********
	
	--**********
	--カーソル
	--**********
	
BEGIN
	
	--移行対象オブジェクトの取得
	--(WF付きドキュメント、WF付きフォルダ、WF付きフォルダ配下のドキュメントは除く)
	insert into IKOU_TARGET_OBJ(seq, id, workflow, ikou_flg)
	with objwf as
	(
		--オブジェクトIDに紐づくステータス
		select
		 distinct
		 est.oid
		,estt.workflow as workflow
		from
		eimst est
		,eimsttype estt
		,eimstver esver
		where
		est.type = estt.id
		and est.sid = esver.sid
		order by oid
	)
	select
		 rownum as seq
		,id
		,workflow
		,ikou_flg
	from
	(
		select
			 EO.id
			,(select OW.workflow from objwf OW where OW.oid = EO.id) as workflow
			,0 as ikou_flg
		from
		eimobj EO
		where
		EO.id in (select distinct oid from eimst)
		and EO.status <> 0
		and
		(
			EO.type in (
				select EOT.id
				from EIMOBJTYPE EOT
				start with EOT.id = (select id from eimobjtype where name = 'ドキュメント')
				connect by prior EOT.id = EOT.parent
			) or
			EO.type in (
				select EOT.id
				from EIMOBJTYPE EOT
				start with EOT.id = (select id from eimobjtype where name = 'ワークスペース')
				connect by prior EOT.id = EOT.parent
			)
		)
		order by id
	);
	
	commit;
	
END;
/
