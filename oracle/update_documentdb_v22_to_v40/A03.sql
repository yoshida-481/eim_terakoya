set serveroutput on
DECLARE
	
	--**************************************************************************
	--定数
	--**************************************************************************
	
	--**************************************************************************
	--変数
	--**************************************************************************
	attTypeId_PublicProcFailure number := 0;
	objTypeId_PDFConvert number := 0;
	cntPDFConvertObject number := 0;
	
	--**************************************************************************
	--カーソル
	--**************************************************************************
	--移行テーブルから
	--移行対象のドキュメント・WF付きフォルダ、を取得
	--WF付きフォルダ配下のドキュメントは除く
	--(ワークフロー履歴作成の移行スクリプトで正常に終了したもの、かつ、
	--オブジェクトの現在のステータスが公開処理中のものが対象)
	cursor c_target_obj is
	select
	OBJST.*
	from
	(
		select
			 X.seq
			,X.id
			,X.workflow
			,(select STT.kind as kind from EIMSTTYPE STT where STT.id = (select ST.type from EIMST ST where ST.oid = X.id and ST.sid = Y.status)) as kind
		from IKOU_TARGET_OBJ X, EIMOBJ Y
		where
		1 = 1
		and X.id = Y.id
		and exists( select 'X' from IKOU_TARGET_WORKFLOW Y where Y.workflow = X.workflow and Y.ikou_flg = 1)
		and ikou_flg = 1
	) OBJST
	where
	1 = 1
	--現在のオブジェクトのステータスが公開処理中のもの
	and OBJST.kind = -13003
	order by seq
	;
	
BEGIN
	--**************************************************************************
	--メイン処理
	--**************************************************************************
	
	--前処理
	select id into attTypeId_PublicProcFailure		from eimattr where name = '公開処理失敗';
	select id into objTypeId_PDFConvert				from eimobjtype where name = 'PDF変換';
	
	FOR r_target_obj IN c_target_obj LOOP
		cntPDFConvertObject := 0;
		
		--PDF変換オブジェクトの存在を確認する
		select count(*) as cnt into cntPDFConvertObject from EIMOBJ where type = objTypeId_PDFConvert and name = to_char(r_target_obj.id);
		
		IF cntPDFConvertObject = 0 THEN
			--オブジェクトの公開処理失敗属性値に1を設定する
			insert into EIMOBJINT values(r_target_obj.id, attTypeId_PublicProcFailure, 0, 1);
		END IF;
		
	END LOOP;
	
	commit;
	
END;
/
