DECLARE
	--**********
	--定数
	--**********
	IS_NOT_MULTIPLE	constant number := 0;
	IS_MULTIPLE 	constant number := 1;

	--**********
	--カーソル
	--**********
	--移行対象のワークフローとその公開処理中のステータスタイプを取得する
	cursor c_workflow is
		select
			 X.workflow as wfid
			,(select Y.name from eimwf Y where Y.id = X.workflow) as wfname
			,X.id as sttid
			,X.name as sttname
		from
		eimsttype X
		where
		X.workflow in
		(
			select
			workflow
			from
			ikou_target_workflow
			where
			ikou_flg = 1
		)
		and X.kind = -13003
		order by X.workflow, X.id
	;
	
	--**********
	--カーソル
	--**********
	--公開通知先エントリー
	objTypeId_PublicEntry number := 0;
	
	objTypeId_WorkflowSetting number := 0;
	--ワークフロー設定承認依頼先デフォルト設定フラグ
	attTypeId_DefaultSettingFlag number := 0;
	--ワークフロー設定メール通知方法のデフォルト設定
	attTypeId_EmailNoticeMethod number := 0;
	--ワークフロー設定差戻し・取戻しメール通知フラグ
	attTypeId_EmailNoticeFlag number := 0;
	--ワークフロー設定処理待ちポップアップ通知フラグ
	attTypeId_WaitingPopupFlag number := 0;
	--ワークフロー設定公開通知先デフォルト設定フラグ
	attTypeId_DefaultNotifiedFlag number := 0;
	
	--ワークフロー公開処理
	objTypeId_WorkflowExhibition number := 0;
	
	--ワークフロー公開処理PDF変換実施フラグ
	attTypeId_PDFConversionFlag number;
	--ワークフロー公開処理有効期限設定期間数字
	attTypeId_DateSettingPrdNum number;
	--ワークフロー公開処理有効期限設定期間単位
	attTypeId_DateSettingPrdUnit varchar2(255);
	--ワークフロー公開処理PDF分割実施フラグ
	attTypeId_PDFSplitFlag number;
	--ワークフロー公開処理PDF署名実施フラグ
	attTypeId_PDFSignatureFlag number;
	--署名有無
	attTypeId_SignatureFlag number;
	--承認日付挿入
	attTypeId_InsertApproveDate number;
	--承認者名挿入
	attTypeId_InsertApproverFlag number;
	--挿入ページ
	attTypeId_InsertPage number;
	--挿入位置基準点
	attTypeId_InsertPlace number;
	--挿入位置座標X
	attTypeId_InsertPlaceX number;
	--挿入位置座標Y
	attTypeId_InsertPlaceY number;
	--セキュリティ設定有無
	attTypeId_SetSecurityFlag number;
	--セキュリティパスワード設定有無
	attTypeId_SetSecurityPWFlag number;
	--セキュリティパスワード
	attTypeId_SecurityPassword varchar2(255);
	--参照用パスワード設定有無
	attTypeId_SetReadPWFlag number;
	--参照用パスワード
	attTypeId_ReadPassword varchar2(255);
	--印刷許可設定
	attTypeId_AcceptPrintFlag number;
	--編集許可設定
	attTypeId_AcceptEditFlag number;
	--注釈追加許可設定
	attTypeId_AcceptAddNoteFlag number;
	--転載許可設定
	attTypeId_AcceptReprintFlag number;
	
	objName varchar2(255);
	objId number;
	rscode number;
	
BEGIN
	
	--公開通知先エントリー
	select id into objTypeId_PublicEntry from EIMOBJTYPE where name = '公開通知先エントリー';
	
	--ワークフロー設定
	select id into objTypeId_WorkflowSetting from EIMOBJTYPE where name = 'ワークフロー設定';
	select id into attTypeId_DefaultSettingFlag from EIMATTR where name = 'ワークフロー設定承認依頼先デフォルト設定フラグ';
	--2
	select id into attTypeId_EmailNoticeMethod from EIMATTR where name = 'ワークフロー設定メール通知方法のデフォルト設定';
	select id into attTypeId_EmailNoticeFlag from EIMATTR where name = 'ワークフロー設定差戻し・取戻しメール通知フラグ';
	select id into attTypeId_WaitingPopupFlag from EIMATTR where name = 'ワークフロー設定処理待ちポップアップ通知フラグ';
	select id into attTypeId_DefaultNotifiedFlag from EIMATTR where name = 'ワークフロー設定公開通知先デフォルト設定フラグ';
	
	--ワークフロー公開処理
	select id into objTypeId_WorkflowExhibition from EIMOBJTYPE where name = 'ワークフロー公開処理';
	select id into attTypeId_PDFConversionFlag from EIMATTR where name = 'ワークフロー公開処理PDF変換実施フラグ';
	select id into attTypeId_DateSettingPrdNum from EIMATTR where name = 'ワークフロー公開処理有効期限設定期間数字';
	select id into attTypeId_DateSettingPrdUnit from EIMATTR where name = 'ワークフロー公開処理有効期限設定期間単位';
	select id into attTypeId_PDFSplitFlag from EIMATTR where name = 'ワークフロー公開処理PDF分割実施フラグ';
	select id into attTypeId_PDFSignatureFlag from EIMATTR where name = 'ワークフロー公開処理PDF署名実施フラグ';
	select id into attTypeId_SignatureFlag from EIMATTR where name = '署名有無';
	select id into attTypeId_InsertApproveDate from EIMATTR where name = '承認日付挿入';
	select id into attTypeId_InsertApproverFlag from EIMATTR where name = '承認者名挿入';
	select id into attTypeId_InsertPage from EIMATTR where name = '挿入ページ';
	select id into attTypeId_InsertPlace from EIMATTR where name = '挿入位置基準点';
	select id into attTypeId_InsertPlaceX from EIMATTR where name = '挿入位置座標X';
	select id into attTypeId_InsertPlaceY from EIMATTR where name = '挿入位置座標Y';
	select id into attTypeId_SetSecurityFlag from EIMATTR where name = 'セキュリティ設定有無';
	select id into attTypeId_SetSecurityPWFlag from EIMATTR where name = 'セキュリティパスワード設定有無';
	select id into attTypeId_SecurityPassword from EIMATTR where name = 'セキュリティパスワード';
	select id into attTypeId_SetReadPWFlag from EIMATTR where name = '参照用パスワード設定有無';
	select id into attTypeId_ReadPassword from EIMATTR where name = '参照用パスワード';
	select id into attTypeId_AcceptPrintFlag from EIMATTR where name = '印刷許可設定';
	select id into attTypeId_AcceptEditFlag from EIMATTR where name = '編集許可設定';
	select id into attTypeId_AcceptAddNoteFlag from EIMATTR where name = '注釈追加許可設定';
	select id into attTypeId_AcceptReprintFlag from EIMATTR where name = '転載許可設定';
	
	/*----------------------------------------------------*/
	--ワークフロー設定オブジェクトを作成する
	/*----------------------------------------------------*/
	FOR r_workflow IN c_workflow LOOP
		--ワークフローID
		objName := to_char(r_workflow.wfid);
		
		--■■■ワークフローIDで公開通知先エントリーオブジェクトを作成する
		ObjectUtils.createObject(1, null, objTypeId_PublicEntry, objName, 0, objId, rscode);
		
		--■■■ワークフローIDでワークフロー設定オブジェクト作成する
		ObjectUtils.createObject(1, null, objTypeId_WorkflowSetting, objName, 0, objId, rscode);
		--■ワークフロー設定承認依頼先デフォルト設定フラグ
		--OFF
		insert into EIMOBJINT values(objId, attTypeId_DefaultSettingFlag, 0, 0);
		--■ワークフロー設定メール通知方法のデフォルト設定
		--OFF
		insert into EIMOBJSTR values(objId, attTypeId_EmailNoticeMethod, 0, 'off');
		--■ワークフロー設定差戻し・取戻しメール通知フラグ
		--ON
		insert into EIMOBJINT values(objId, attTypeId_EmailNoticeFlag, 0, 1);
		--■ワークフロー設定処理待ちポップアップ通知フラグ
		--ON
		insert into EIMOBJINT values(objId, attTypeId_WaitingPopupFlag, 0, 1);
		--■ワークフロー設定公開通知先デフォルト設定フラグ
		--OFF
		insert into EIMOBJINT values(objId, attTypeId_DefaultNotifiedFlag, 0, 0);
		
		--公開処理中ステータスタイプID
		objName := to_char(r_workflow.sttid);
		
		--■■■公開処理中ステータスタイプIDでワークフロー公開処理オブジェクトを作成する
		ObjectUtils.createObject(1, null, objTypeId_WorkflowExhibition, objName, 0, objId, rscode);
		--ON ワークフロー公開処理PDF変換実施フラグ
		insert into EIMOBJINT values(objId, attTypeId_PDFConversionFlag, 0, 1);
		
		--ワークフロー公開処理有効期限設定期間数字 属性値を設定しない
		--insert into EIMOBJINT values(objId, attTypeId_DateSettingPrdNum, 0, 0);
		--ワークフロー公開処理有効期限設定期間単位 属性値を設定しない
		--insert into EIMOBJSTR values(objId, attTypeId_DateSettingPrdUnit, 0, 0);
		
		insert into EIMOBJINT values(objId, attTypeId_PDFSplitFlag, 0, 0);
		insert into EIMOBJINT values(objId, attTypeId_PDFSignatureFlag, 0, 0);
		insert into EIMOBJINT values(objId, attTypeId_SignatureFlag, 0, 0);
		insert into EIMOBJINT values(objId, attTypeId_InsertApproveDate, 0, 0);
		insert into EIMOBJINT values(objId, attTypeId_InsertApproverFlag, 0, 0);
		insert into EIMOBJINT values(objId, attTypeId_InsertPage, 0, 0);
		insert into EIMOBJINT values(objId, attTypeId_InsertPlace, 0, 0);
		insert into EIMOBJINT values(objId, attTypeId_InsertPlaceX, 0, 0);
		insert into EIMOBJINT values(objId, attTypeId_InsertPlaceY, 0, 0);
		insert into EIMOBJINT values(objId, attTypeId_SetSecurityFlag, 0, 0);
		insert into EIMOBJINT values(objId, attTypeId_SetSecurityPWFlag, 0, 0);
		
		--セキュリティパスワード 属性値を設定しない
		--insert into EIMOBJSTR values(objId, attTypeId_SecurityPassword, 0, 0);
		
		insert into EIMOBJINT values(objId, attTypeId_SetReadPWFlag, 0, 0);
		
		--参照用パスワード 属性値を設定しない
		--insert into EIMOBJSTR values(objId, attTypeId_ReadPassword, 0, 0);
		
		insert into EIMOBJINT values(objId, attTypeId_AcceptPrintFlag, 0, 0);
		insert into EIMOBJINT values(objId, attTypeId_AcceptEditFlag, 0, 0);
		insert into EIMOBJINT values(objId, attTypeId_AcceptAddNoteFlag, 0, 0);
		insert into EIMOBJINT values(objId, attTypeId_AcceptReprintFlag, 0, 0);
		
		
	END LOOP;
	
	commit;
	
END;
/
