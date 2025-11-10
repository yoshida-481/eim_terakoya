//=====================================================================
// EIMANAGER 文書管理
// 文書管理用カスタム属性にUIコントロールを設定するスクリプト
// 
// ※実際に実行する際は次の点を確認してください
// ※・UIコントロール設定除外ネームスペースマップにUIコントロールを設定しないネームスペースを設定していること
// ※・UIコントロール設定除外属性タイプ名称マップにUIコントロールを設定しない属性タイプ名を設定していること
// ※・DB接続先が設定ファイル(config.propertoes)に定義されていること
//=====================================================================

importPackage(java.lang);
importPackage(java.util);

importPackage(Packages.jp.co.ctc_g.eim.framework2.common.context);
importPackage(Packages.jp.co.ctc_g.eim.framework2.common.enumeration);
importPackage(Packages.jp.co.ctc_g.eim.framework2.common.util);

importPackage(Packages.jp.co.ctc_g.eim.framework2.business.domain.criteria);
importPackage(Packages.jp.co.ctc_g.eim.framework2.business.domain.entity);
importPackage(Packages.jp.co.ctc_g.eim.framework2.business.domain.search);
importPackage(Packages.jp.co.ctc_g.eim.framework2.business.service);

var tx;
var context;

// UIコントロール設定除外ネームスペースマップ
var exclusionNamespaceMap =
{
	"app.form.user": "app.form.user",
	"app.form.dev": "app.form.dev",
};

// UIコントロール設定除外属性タイプ名称マップ
// キー：属性タイプ名称、値：属性タイプ名称
var exclusionAttributeTypeNameMap =
{
	"連続データID": "連続データID",
	"属性タイプ値マスター数値リスト": "属性タイプ値マスター数値リスト",
	"属性タイプ値マスター日付値リスト": "属性タイプ値マスター日付値リスト",
	"属性タイプ値マスター文字列値リスト": "属性タイプ値マスター文字列値リスト",
	"属性タイプ値マスターテキスト値リスト": "属性タイプ値マスターテキスト値リスト",
	"属性タイプ値マスターダブル数字リスト": "属性タイプ値マスターダブル数字リスト",
	"属性タイプ値マスター表示色リスト": "属性タイプ値マスター表示色リスト",
	"属性タイプ値マスター表示設定リスト": "属性タイプ値マスター表示設定リスト",
	"属性ツリー分類対象": "属性ツリー分類対象",
	"属性ツリー他言語ID": "属性ツリー他言語ID",
	"属性ツリー他言語名称": "属性ツリー他言語名称",
	"属性ツリー所属属性ID": "属性ツリー所属属性ID",
	"属性ツリー所属属性ポジション": "属性ツリー所属属性ポジション",
	"属性ツリー所属属性値なし表示フラグ": "属性ツリー所属属性値なし表示フラグ",
	"ワークフロー設定承認依頼先デフォルト設定フラグ": "ワークフロー設定承認依頼先デフォルト設定フラグ",
	"ワークフロー設定メール通知方法のデフォルト設定": "ワークフロー設定メール通知方法のデフォルト設定",
	"ワークフロー設定差戻し・取戻しメール通知フラグ": "ワークフロー設定差戻し・取戻しメール通知フラグ",
	"ワークフロー設定処理待ちポップアップ通知フラグ": "ワークフロー設定処理待ちポップアップ通知フラグ",
	"ワークフロー設定上長承認設定": "ワークフロー設定上長承認設定",
	"ワークフロー公開処理PDF変換実施フラグ": "ワークフロー公開処理PDF変換実施フラグ",
	"ワークフロー公開処理有効期限設定期間数字": "ワークフロー公開処理有効期限設定期間数字",
	"ワークフロー公開処理有効期限設定期間単位": "ワークフロー公開処理有効期限設定期間単位",
	"パス": "パス",
	//"プロパティ": "プロパティ",
	//"改訂内容": "改訂内容",
	"作成者": "作成者",
	//"有効期限": "有効期限",
	"コメント": "コメント",
	"受信確認": "受信確認",
	"更新者": "更新者",
	"更新日": "更新日",
	"作成日": "作成日",
	"サイズ": "サイズ",
	"前セキュリティ": "前セキュリティ",
	"上位からの引継ぎ属性": "上位からの引継ぎ属性",
	"上位WFフォルダ": "上位WFフォルダ",
	"公開処理失敗": "公開処理失敗",
	"名称割当て属性": "名称割当て属性",
	"下位への引継ぎ属性": "下位への引継ぎ属性",
	"下位フォルダ管理セキュリティ": "下位フォルダ管理セキュリティ",
	"ワークフロー設定公開通知先デフォルト設定フラグ": "ワークフロー設定公開通知先デフォルト設定フラグ",
	"ワークフロー公開処理PDF分割実施フラグ": "ワークフロー公開処理PDF分割実施フラグ",
	"ワークフロー公開処理PDF署名実施フラグ": "ワークフロー公開処理PDF署名実施フラグ",
	"エントリータイプID": "エントリータイプID",
	"エントリー対象ID": "エントリー対象ID",
	"ドキュメントタイプID": "ドキュメントタイプID",
	"親オブジェクトID": "親オブジェクトID",
	"結合対象オブジェクトID": "結合対象オブジェクトID",
	"登録者": "登録者",
	"PDF署名ステータス": "PDF署名ステータス",
	"署名有無": "署名有無",
	"承認日付挿入": "承認日付挿入",
	"承認者名挿入": "承認者名挿入",
	"挿入ページ": "挿入ページ",
	"挿入位置基準点": "挿入位置基準点",
	"挿入位置座標X": "挿入位置座標X",
	"挿入位置座標Y": "挿入位置座標Y",
	"セキュリティ設定有無": "セキュリティ設定有無",
	"セキュリティパスワード設定有無": "セキュリティパスワード設定有無",
	"セキュリティパスワード": "セキュリティパスワード",
	"参照用パスワード設定有無": "参照用パスワード設定有無",
	"参照用パスワード": "参照用パスワード",
	"印刷許可設定": "印刷許可設定",
	"編集許可設定": "編集許可設定",
	"注釈追加許可設定": "注釈追加許可設定",
	"転載許可設定": "転載許可設定",
	"PDF結合処理失敗": "PDF結合処理失敗",
	"PDF分割状態": "PDF分割状態",
	"リンク先": "リンク先",
	"ドキュメントリンク": "ドキュメントリンク",
	"ブランチ先削除": "ブランチ先削除",
	"対象ドキュメント": "対象ドキュメント",
	"実施者": "実施者",
	"選択タグ": "選択タグ",
	"タグ": "タグ",
	"タグ付与者": "タグ付与者",
	"タグ付与日": "タグ付与日",
	"署名・暗号化状態": "署名・暗号化状態",
	"署名・暗号化バージョン": "署名・暗号化バージョン",
	"表示色": "表示色",
	"比較元ドキュメントオブジェクトID": "比較元ドキュメントオブジェクトID",
	"比較対象ドキュメントオブジェクトID": "比較対象ドキュメントオブジェクトID",
	"完了通知メール設定": "完了通知メール設定",
	"レイアウト解析設定": "レイアウト解析設定",
	"ZIPファイル名": "ZIPファイル名",
	"登録先パス": "登録先パス",
	"公開通知タイミング": "公開通知タイミング",
	"承認依頼通知タイミング": "承認依頼通知タイミング",
	"公開通知送信先": "公開通知送信先",
	"使用可能ドキュメントタイプ絞込みフラグ": "使用可能ドキュメントタイプ絞込みフラグ",
	"使用可能ドキュメントタイプ": "使用可能ドキュメントタイプ",
	"使用可能フォルダタイプ絞込みフラグ": "使用可能フォルダタイプ絞込みフラグ",
	"使用可能フォルダタイプ": "使用可能フォルダタイプ",
	"使用可能タグタイプ絞込みフラグ": "使用可能タグタイプ絞込みフラグ",
	"使用可能タグタイプ": "使用可能タグタイプ",
	"使用可能セキュリティ絞込みフラグ": "使用可能セキュリティ絞込みフラグ",
	"使用可能セキュリティ": "使用可能セキュリティ",
	"責任者": "責任者",
	"責任者種別": "責任者種別",
	"リンク更新タイミング": "リンク更新タイミング",
	"OCR処理ステータス": "OCR処理ステータス",
	"OCR結果ステータス": "OCR結果ステータス",
	"OCR設定有無": "OCR設定有無",
	"番号": "番号",
	"WebDAVロックフラグ": "WebDAVロックフラグ",
	"リビジョンアップ引継ぎ": "リビジョンアップ引継ぎ",
	"最新リビジョン関連付け": "最新リビジョン関連付け",
	"選択カスタマイズテーブル名称": "選択カスタマイズテーブル名称",
	"ワークフロー公開処理URL挿入フラグ": "ワークフロー公開処理URL挿入フラグ",
	"文書ID": "文書ID",
	"削除日時": "削除日時",
	"公開通知コメント": "公開通知コメント",
	"公開通知コメントログ": "公開通知コメントログ",
	"上長のみ表示デフォルト設定ステータス": "上長のみ表示デフォルト設定ステータス",
	"PDF変換処理実行日時": "PDF変換処理実行日時",
	"公開PDF事前登録日時": "公開PDF事前登録日時",
	"スキップステータスタイプID": "スキップステータスタイプID",
	"手動削除禁止フラグ": "手動削除禁止フラグ",
	"ユーザ別Box連携利用許可フラグ": "ユーザ別Box連携利用許可フラグ",
	"サムネイル変換不可フラグ": "サムネイル変換不可フラグ",
	"プレビュー変換不可フラグ": "プレビュー変換不可フラグ"
};

//---------------------------------------------------------------------
// オブジェクトタイプ取得
//---------------------------------------------------------------------
function getObjectType(name) {

	var objectTypeService = contxt.getBean("objectTypeService2");

	var objectType = new ObjectTypeDomain();
	objectType.setDefinitionName(name);

	return objectTypeService.getByDefinitionName(name);
}

//---------------------------------------------------------------------
// オブジェクト作成
//---------------------------------------------------------------------
function createObject(objectName, objectType, duplicateCheckMode) {

	var objectService = contxt.getBean("objectService2");

	var object = new ObjectDomain();
	object.setName(objectName);
	object.setType(objectType);

	var newObject = objectService.create(object, duplicateCheckMode);

	return newObject;
}

//---------------------------------------------------------------------
//オブジェクト取得(タイプと名称を指定)
//---------------------------------------------------------------------
function getByTypeAndName(objectType, objctName) {
	
	var objectService = contxt.getBean("objectService2");
	
	return objectService.getByTypeAndName(objectType, objctName);
}

//---------------------------------------------------------------------
// オブジェクトの属性値(単一数値型)設定
//---------------------------------------------------------------------
function setAttributeSingleLong(object, attributeType, value) {

	var objectService = contxt.getBean("objectService2");

	objectService.setAttributeSingleLong(object, attributeType, value);
}
//---------------------------------------------------------------------
// オブジェクトの属性値(単一文字列型)設定
//---------------------------------------------------------------------
function setAttributeSingleString(object, attributeType, value) {

	var objectService = contxt.getBean("objectService2");

	objectService.setAttributeSingleString(object, attributeType, value);

}

//---------------------------------------------------------------------
// 全属性タイプ取得
//---------------------------------------------------------------------
function getAllAttributeTypeList() {

	var attributeTypeService = contxt.getBean("attributeTypeService2");

	var attributeTypeCriteria = new AttributeTypeCriteria();

	return attributeTypeService.getList(attributeTypeCriteria);
}

//---------------------------------------------------------------------
// 属性タイプ取得
//---------------------------------------------------------------------
function getAttributeType(name) {

	var attributeTypeService = contxt.getBean("attributeTypeService2");

	var attributeType = new AttributeTypeDomain();
	attributeType.setDefinitionName(name);

	return attributeTypeService.getByDefinitionName(name);
}

//---------------------------------------------------------------------
// UIコントロール設定対象属性タイプ判定
//---------------------------------------------------------------------
function isSettingTarget(attributeType) {
	var definitionName = attributeType.getDefinitionName();
	
	// ネームスペース
	for (key in exclusionNamespaceMap) {
		if (definitionName.indexOf(key) == 0) {
			// 文字列の先頭で一致した場合、対象外
			return false;
		}
	}
	
	// 属性タイプ名
	for (key in exclusionAttributeTypeNameMap) {
		if (definitionName == key) {
			// 完全一致した場合、対象外
			return false;
		}
	}
	
	return true;
}

//---------------------------------------------------------------------
// メイン処理
//---------------------------------------------------------------------
try {

	print("開始します…");

	var contextFiles = new Array("applicationContext-setLayoutToDocumentAttribute.xml");
	ApplicationContextLoader.init(contextFiles);

	contxt = ApplicationContextLoader.getApplicationContext();
	print("contxt - " + contxt);

	// DBコネクションの確立
	// config.propertiesに設定されている接続情報を利用する。
	tx = new TransactionContext(ConnectionModeEnum.CONSOLE, new UserDomain(1), "JA");
	EIMThreadContext.putTransactionContext(tx);

	// 「属性タイプ」オブジェクトタイプ取得
	var attributeTypeObjType = getObjectType("app.form.dev:属性タイプ");

	// 「属性タイプ」取得
	var visibleAttrType = getAttributeType("app.form.dev:可視性");
	var uIContorolTypeAttrType = getAttributeType("app.form.dev:UIコントロール区分");
	var uIContorolIDAttrType = getAttributeType("app.form.dev:UIコントロールID");
	
	// 全属性タイプ取得
	var allAttributeTypeList = getAllAttributeTypeList();
	
	for (var i = 0; i < allAttributeTypeList.size(); i++) {
		
		var attributeType = allAttributeTypeList.get(i);
		// 対象外の場合、UIコントロールを設定しない
		if (!isSettingTarget(attributeType)) continue;
		
		var attributeTypeObject = getByTypeAndName(attributeTypeObjType, attributeType.getId().toFixed());
		
		if (attributeTypeObject != null) continue;
		
		var valueType = attributeType.getValueType();
		
		var uIContorolType = null;
		var uIContorolID = null;
		if (valueType == ValueTypeEnum.LONG) {
			uIContorolType = "TextInput";
			uIContorolID = "uIControlTextInput";
		} else if (valueType == ValueTypeEnum.STRING) {
			uIContorolType = "TextInput";
			uIContorolID = "uIControlTextInput";
		} else if (valueType == ValueTypeEnum.DATE) {
			uIContorolType = "Calender";
			uIContorolID = "uIControlDateField";
		} else if (valueType == ValueTypeEnum.TEXT) {
			uIContorolType = "TextArea";
			uIContorolID = "uIControlTextArea";
		} else if (valueType == ValueTypeEnum.DOUBLE) {
			uIContorolType = "TextInput";
			uIContorolID = "uIControlTextInput";
		} else if (attributeType.getDefinitionName() == "関連ドキュメント"){
			uIContorolType = "ObjectSearch"
			uIContorolID = "uIControlSearchDocumentField"
		} else if (attributeType.getDefinitionName() == "添付ファイル"){
			uIContorolType = "File"
			uIContorolID = "uIControlFile"
		} else {
			// 上記以外のデータ型は文書管理では利用していなのでUIコントロールを設定しない
			continue;
		}
		
		// UIコントロールを設定する
		attributeTypeObject = createObject(attributeType.getId(), attributeTypeObjType, DuplicateCheckModeEnum.TYPE);
		setAttributeSingleLong(attributeTypeObject, visibleAttrType, 1);
		
		setAttributeSingleString(attributeTypeObject, uIContorolTypeAttrType, uIContorolType);
		setAttributeSingleString(attributeTypeObject, uIContorolIDAttrType, uIContorolID);
		
		print(attributeType.getDefinitionName() + " " + uIContorolType + " " + uIContorolID + " " + "OK");
	}
	
	
	//コミット
	//tx.getDBConnection().commit();
	tx.getDBConnection().rollback();
	tx.getDBConnection().close();
	print("正常にデータが登録されました。");

} catch (e) {

	if (tx != null && tx.getDBConnection() != null) {
		tx.getDBConnection().rollback();
		tx.getDBConnection().close();
	}
	throw e;
}
