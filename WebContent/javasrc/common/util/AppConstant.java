package common.util;

/**
*
* アプリケーション側定数定義クラス
*
*/
public interface AppConstant {


	/* --- システム共通定義 -------------------------------------- */

	/**
	 * 言語IDの値(日本語)
	 */
	public static final String LANG_VALUE_JA = "JA";

	/**
	 * 言語IDの値(英語)
	 */
	public static final String LANG_VALUE_EN = "EN";
	/**
	 * ステータスなし公開　改定中
	 *
	 * <li>チェックアウト後(最新レビジョンのドキュメントが作られる)、かつ、最新ドキュメントにチェックイン前の状態において、
	 * 最新ドキュメントよりレビジョンが1つ前のドキュメントを取得する
	 * @deprecated V6.17時点未使用。性能劣化の可能性あり。
	 */
	public static final String NOSTATAS_PUBLIC_EDIT_OBJSQL = " select eimobj.id as ID "+
    " from "+
    " eimobj, eimver, "+
    " (select oob.id as LID,vo.vid, oob.rev "+
    "  from "+
      "  eimobj oob, " +
       " eimver vo, " +
       " (select eimver.vid ,max(rev) as rev "+
        "  from "+
 "  eimobj iob, "
			+
           " eimver "+
         " where "+
         "   eimver.oid = iob.id group by eimver.vid "+
        " ) vv "+
         "where "+
         " oob.id = vo.oid and vv.vid = vo.vid "+
         " and oob.rev = vv.rev "+
         " and oob.status = 0 and oob.rev > 0 "+
    " ) LO "+
   " where "+
    "  eimobj.id = eimver.oid "+
     " and LO.rev - 1 = eimobj.rev "+
     " and eimver.vid = Lo.vid "+
     " and eimobj.luser > 0 ";

	/**
	 * ステータスなし公開　過去レビジョンを全て取得
	 * @deprecated V6.17時点未使用。性能劣化の可能性あり。
	 */
	public static final String NOSTATAS_PUBLIC_OBJSQL_FOR_PASTDOC = " select eimobj.id as ID "+
    " from "+
    " eimobj, eimver, "+
    " (select oob.id as LID,vo.vid, oob.rev "+
    "  from "+
      "  eimobj oob, " +
       " eimver vo, " +
       " (select eimver.vid ,max(rev) as rev "+
        "  from "+
 "  eimobj iob, "
			+
           " eimver "+
         " where "+
         "   eimver.oid = iob.id group by eimver.vid "+
        " ) vv "+
         "where "+
         " oob.id = vo.oid and vv.vid = vo.vid "+
         " and oob.rev = vv.rev "+
        " and oob.rev > 0 "+
    " ) LO "+
   " where "+
    "  eimobj.id = eimver.oid "+
     " and LO.rev - 1 >= eimobj.rev "+	// 過去のレビジョン全て
     " and eimver.vid = Lo.vid ";


		/**
	 * ステータスなし公開
	 *
	 * <li>1つ目のselect 改定なし (最新をチェックイン済みの状態の最新のドキュメントを取得) <li>2つのselect 初期登録
	 * (0版で最新版のドキュメントを取得)
	 * @deprecated V6.17時点未使用。性能劣化の可能性あり。
	 */
	public static final String NOSTATAS_PUBLIC_OBJSQL_UNION_FOR_LATESTDOC_AND_INITDOC =
 " union "+
 " select eimobj.id as ID "+
  "  from "+
    "  eimobj, eimver, "+
     " (select oob.id as LID,vo.vid, oob.rev "+
       " from "+
 " eimobj oob, "
			+
         " eimver vo, "+
         " (select eimver.vid ,max(rev)-1 as rev "+
           " from "+
            "  eimobj iob, "+
             " eimver "+
            " where "+
            "  eimver.oid = iob.id group by eimver.vid "+
         " ) vv "+
          " where "+
           " oob.id = vo.oid and vv.vid = vo.vid "+
           " and oob.rev = vv.rev "+
           " and oob.latest = 0 "+
      " ) LB "+
      " where " +
      "  eimobj.id = eimver.oid "+
 " and LB.rev + 1 = eimobj.rev "
			+
       " and eimobj.latest = 1 "+
       " and eimver.vid = LB.vid "+
  " union "+
" select id "+
  "  from eimobj "+
   " where "+
     " rev = 0 "+
     " and latest = 1 "+
     " and luser is null "+
     " and status = 0 ";

	/**
	 * ステータスなし公開
	 *
	 * <li>1つ目のselect 改定中 <li>2つ目のselect 改定なし <li>3つのselect 初期登録
	 * @deprecated V6.17時点未使用。性能劣化の可能性あり。
	 */
	public static final String NOSTATAS_PUBLIC_OBJSQL= " select ID from "+
		" ("+
			NOSTATAS_PUBLIC_EDIT_OBJSQL +
			NOSTATAS_PUBLIC_OBJSQL_UNION_FOR_LATESTDOC_AND_INITDOC +
			" ) sub ";

	/**
	 * (公開アイコン表示判定用) ステータスなし公開
	 *
	 * <li>1つ目のselect 改定中 過去レビジョンを全て取得 <li>2つ目のselect 改定なし <li>3つのselect 初期登録
	 * @deprecated V6.17時点未使用。性能劣化の可能性あり。
	 */
	public static final String NOSTATAS_PUBLIC_ICON_OBJSQL= " select ID from "+
		" ("+
			NOSTATAS_PUBLIC_OBJSQL_FOR_PASTDOC +
			NOSTATAS_PUBLIC_OBJSQL_UNION_FOR_LATESTDOC_AND_INITDOC +
			" ) sub ";

	/**
	* doubleの値のフォーマット
	* Double.MAX_VALUE E308
	* Double.MIN_VALUE E-324
	* 有効桁数は15桁ですが、保持できる値までフォーマットとして用意します。
	*/
	public static final String DOUBLE_MAX_STRING_FORMAT = "##################################################" + "##################################################"+
													"##################################################" + "##################################################"+
													"##################################################" + "##################################################" +
													"########" + "." +
													"##################################################" + "##################################################" +
													"##################################################" + "##################################################" +
													"##################################################" + "##################################################" +
													"########################" ;
	/**
	 * ドキュメント管理 必須属性リスト
	 *
	 * <li>必須属性とは、create_documentdb.sqlで初期値として登録する属性の意。
	 */
	public static final String[] ESSENTIAL_ATTRIBUTE_DEFNAME = {
											"パス",
											"プロパティ",
											"改訂内容",
											"作成者",
											"有効期限",
											"通知タイミング",
											"コメント",
											"受信確認",
											"承認依頼者",
											"承認依頼日",
											"承認者",
											"承認日",
											"ステータス",
											"通知先種別：ID",
											"被承認依頼者種別：ID",
											"再承認依頼先種別：ID",
											"差戻しユーザ",
											"更新者",
											"更新日",
											"作成日",
											"サイズ",
											"属性タイプ値マスター数値リスト",
											"属性タイプ値マスター日付値リスト",
											"属性タイプ値マスター文字列値リスト",
											"属性タイプ値マスターテキスト値リスト",
											"属性タイプ値マスター表示色リスト",
											"属性タイプ値マスター表示設定リスト",
											"属性ツリー分類対象",
											"属性ツリー他言語ID",
											"属性ツリー他言語名称",
											"属性ツリー所属属性ID",
											"属性ツリー所属属性ポジション",
											"属性ツリー所属属性値なし表示フラグ",
											"ワークフロー設定上長承認設定",
											"ワークフロー設定承認依頼先デフォルト設定フラグ",
											"ワークフロー設定メール通知方法のデフォルト設定",
											"ワークフロー設定差戻し・取戻しメール通知フラグ",
											"ワークフロー設定処理待ちポップアップ通知フラグ",
											"ワークフロー公開処理PDF変換実施フラグ",
											"ワークフロー公開処理有効期限設定期間数字",
											"ワークフロー公開処理有効期限設定期間単位",
											"前セキュリティ",
											"上位からの引継ぎ属性",
											"上位WFフォルダ",
											"公開処理失敗",
											"名称割当て属性",
											"下位への引継ぎ属性",
											"下位フォルダ管理セキュリティ",
											"ワークフロー設定公開通知先デフォルト設定フラグ",
											"ワークフロー公開処理PDF分割実施フラグ",
											"ワークフロー公開処理PDF署名実施フラグ",
											"エントリータイプID",
											"エントリー対象ID",
											"ドキュメントタイプID",
											"親オブジェクトID",
											"結合対象オブジェクトID",
											"登録者",
											"PDF署名ステータス",
											"署名有無",
											"承認日付挿入",
											"承認者名挿入",
											"挿入ページ",
											"挿入位置基準点",
											"挿入位置座標X",
											"挿入位置座標Y",
											"セキュリティ設定有無",
											"セキュリティパスワード設定有無",
											"セキュリティパスワード",
											"参照用パスワード設定有無",
											"参照用パスワード",
											"印刷許可設定",
											"編集許可設定",
											"注釈追加許可設定",
											"転載許可設定",
											"PDF結合処理失敗",
											"PDF分割状態",
											"リンク先",
											"ドキュメントリンク",
											"ブランチ先削除",
											"対象ドキュメント",
											"実施者",
											"選択タグ",
											"タグ",
											"タグ付与者",
											"タグ付与日",
											"署名・暗号化状態",
											"署名・暗号化バージョン",
											"表示色",
											"比較元ドキュメントオブジェクトID",
											"比較対象ドキュメントオブジェクトID",
											"完了通知メール設定",
											"レイアウト解析設定",
											"ZIPファイル名",
											"登録先パス",
											"属性タイプ値マスターダブル数字リスト",
											"使用可能ドキュメントタイプ絞込みフラグ",
											"使用可能ドキュメントタイプ",
											"使用可能フォルダタイプ絞込みフラグ",
											"使用可能フォルダタイプ",
											"使用可能タグタイプ絞込みフラグ",
											"使用可能タグタイプ",
											"使用可能セキュリティ絞込みフラグ",
											"使用可能セキュリティ",
											"責任者",
											"責任者種別",
											"リンク更新タイミング",
											"OCR処理ステータス",
											"OCR結果ステータス",
											"OCR設定有無",
											"WebDAVロックフラグ",
											"番号",
											"選択カスタマイズテーブル名称",
											"文書ID",
											"チェックイン可能ステータス",
											"削除日時",
											"公開通知コメント",
											"公開通知コメントログ",
											"PDF変換処理実行日時",
											"スキップステータスタイプID",
											"公開PDF事前登録日時",
											"電子署名用言語",
											"署名用ジョブ名",
											"公開通知テンプレート複合グループID",
											"公開通知テンプレート名称",
											"公開通知テンプレートロールID",
											"公開通知テンプレートユーザID",
											"公開通知テンプレートグループID",
											"サムネイル変換不可フラグ",
											"プレビュー変換不可フラグ"
										};


	/**
	 * ドキュメント管理 テーブル非表示属性リスト
	 *
	 * ※ドキュメント系列タイプ/WS系列タイプに割り当てられている属性のうち
	 *   非表示にすべき管理用属性リスト
	 */
	public static final String[] NONTABLE_ATTRIBUTE_DEFNAME = {
											"前セキュリティ",
											"上位からの引継ぎ属性",
											"上位WFフォルダ",
											"公開処理失敗",
											"名称割当て属性",
											"下位への引継ぎ属性",
											"下位フォルダ管理セキュリティ",
											"PDF結合処理失敗",
											"PDF分割状態",
											"リンク先",
											"ドキュメントリンク",
											"タグ",
											"タグ付与者",
											"タグ付与日",
											"署名・暗号化状態",
											"署名・暗号化バージョン",
											"所属ワークスペース",
											"リンク所属ワークスペース",
											"使用可能ドキュメントタイプ絞込みフラグ",
											"使用可能ドキュメントタイプ",
											"使用可能フォルダタイプ絞込みフラグ",
											"使用可能フォルダタイプ",
											"使用可能タグタイプ絞込みフラグ",
											"使用可能タグタイプ",
											"使用可能セキュリティ絞込みフラグ",
											"使用可能セキュリティ",
											"責任者",
											"責任者種別",
											"参照トップメニュー",
											"リンク更新タイミング",
											"OCR処理ステータス",
											"OCR結果ステータス",
											"OCR設定有無",
											"WebDAVロックフラグ",
											"番号",
											"選択カスタマイズテーブル名称",
											"スキャン用表紙フラグ",
											"スキャン用表紙作成可否フラグ",
											"チェックイン可能ステータス",
											"削除日時",
											"公開通知コメント",
											"公開通知コメントログ",
											"PDF変換処理実行日時",
											"スキップステータスタイプID",
											"公開PDF事前登録日時",
											"手動削除禁止フラグ",
											"サムネイル変換不可フラグ",
											"プレビュー変換不可フラグ"
										};

	/**
	 * ドキュメント管理 システム属性リスト
	 *
	 * ユーザーが直接変更することが好ましくない属性のリスト
	 */
	public static final String[] SYSTEM_ATTRIBUTE_DEFNAME = {
											"パス",
											"作成者",
											"作成日",
											"更新者",
											"更新日",
											"前セキュリティ",
											"上位からの引継ぎ属性",
											"上位WFフォルダ",
											"公開処理失敗",
											"名称割当て属性",
											"下位への引継ぎ属性",
											"下位フォルダ管理セキュリティ",
											"サイズ",
											"PDF結合処理失敗",
											"PDF分割状態",
											"リンク先",
											"ドキュメントリンク",
											"タグ",
											"タグ付与者",
											"タグ付与日",
											"署名・暗号化状態",
											"署名・暗号化バージョン",
											"比較元ドキュメントオブジェクトID",
											"比較対象ドキュメントオブジェクトID",
											"完了通知メール設定",
											"レイアウト解析設定",
											"ZIPファイル名",
											"登録先パス",
											"使用可能ドキュメントタイプ絞込みフラグ",
											"使用可能ドキュメントタイプ",
											"使用可能フォルダタイプ絞込みフラグ",
											"使用可能フォルダタイプ",
											"使用可能タグタイプ絞込みフラグ",
											"使用可能タグタイプ",
											"使用可能セキュリティ絞込みフラグ",
											"使用可能セキュリティ",
											"責任者",
											"責任者種別",
											"リンク更新タイミング",
											"OCR処理ステータス",
											"OCR結果ステータス",
											"WebDAVロックフラグ",
											"番号",
											"OCR設定有無",
											"選択カスタマイズテーブル名称",
											"文書ID",
											"スキャン用表紙フラグ",
											"スキャン用表紙作成可否フラグ",
											"チェックイン可能ステータス",
											"削除日時",
											"公開通知コメント",
											"公開通知コメントログ",
											"PDF変換処理実行日時",
											"スキップステータスタイプID",
											"公開PDF事前登録日時",
											"電子署名用言語",
											"署名用ジョブ名",
											"公開通知テンプレート複合グループID",
											"公開通知テンプレート名称",
											"公開通知テンプレートロールID",
											"公開通知テンプレートユーザID",
											"公開通知テンプレートグループID",
											"手動削除禁止フラグ",
											"サムネイル変換不可フラグ",
											"プレビュー変換不可フラグ"
										};
	/**
	 * csv取り込み 更新対象外属性
	 *
	 * csv取り込みバッチ処理で更新対象外の属性リスト
	 */
	public static final String[] CSV_NOTTARGET_ATTRIBUTE_DEFNAME = {
											"公開"
											, "名称"
											, "履歴"
											, "場所"
											, "ステータスID"
										};

	/**
	 * フレームワーク 初期登録属性リスト
	 *
	 * フレームワークで初期登録される属性のリスト
	 */
	public static final String[] FRAMEWORK_ATTRIBUTE_DEFNAME = {
											"連続データID"
										};

	/**
	 * 属性ツリー管理 属性ツリー非表示属性リスト
	 *
	 * 属性ツリーには選択できないダミー属性のリスト
	 */
	public static final String[] NONATTRTREE_ATTRIBUTE_DEFNAME = {
											"作成者"
											, "作成日"
											, "更新者"
											, "更新日"
											, "サイズ"
											, "所属ワークスペース"
											, "リンク所属ワークスペース"
											, "文書ID"
										};

	/**
	 * コピー時等に継承しない属性リスト
	 */
	public static final String[] NONINHERIT_ATTRIBUTE_DEFNAME = {
											"公開処理失敗",
											"PDF結合処理失敗",
											"PDF分割状態",
											"リンク先",
											"ドキュメントリンク",
											"タグ",
											"タグ付与者",
											"タグ付与日",
											"署名・暗号化状態",
											"署名・暗号化バージョン",
											"OCR処理ステータス",
											"OCR結果ステータス",
											"WebDAVロックフラグ",
											"文書ID",
											"PDF変換処理実行日時",
											"スキップステータスタイプID",
											"公開PDF事前登録日時",
											"サムネイル変換不可フラグ",
											"プレビュー変換不可フラグ"
										};

	/**
	 * 値がNULLになることがない属性リスト
	 */
	public static final String[] NOT_NULL_ATTRIBUTE_DEFNAME = {
											"作成者",
											"作成日",
											"更新者",
											"更新日",
											"パス",
											"文書ID"
										};

	/**
	 * セキュリティ無の設定値
	 */
	public static final int NO_SECUTIRY = 0;

	/**
	 * ドキュメント管理 Windows禁止文字の配列
	 */
	public static final String[] INVALID_NAME_CHAR = { "\\", "/", ":", "*", "?", "\"", "<", ">", "|" };

	/**
	 * 最新履歴フラグ：オン
	 */
	public static final int LATEST_HISTORY_FLAG_ON = 1;

	/**
	 * systemユーザのユーザID
	 */
	public static final int SYSYEM_USER_ID = 1;

	/* --- ファイルダウンロード ----------------------------------------- */

	/**
	 * エンコード文字列："MS932"
	 */
	public static final String ENCODE_MS932 = "MS932";

	/**
	 * エンコード文字列："utf-8"
	 */
	public static final String ENCODE_UTF8 = "utf-8";

	/**
	 * ファイルダウンロードのファイル名エンコード：日本語のみオン
	 */
	public static final String DOWNLOAD_FILENAME_ENCODE_JAP_ONLY_ON = "on";


	/* --- アプリケーション種別 ------------------------------------------- */

	/**
	 * アプリケーション種別ID：システム管理
	 */
	public static final String SYSTEM = "1";
	/**
	 * アプリケーション種別ID：ドキュメント管理
	 */
	public static final String DOCUMENT = "2";

	/**
	 * システム管理アプリケーション種別ID：汎用
	 */
	public static final String ADMIN_APP_ID_GENERAL = "general";
	/**
	 * システム管理アプリケーション種別ID：ドキュメント管理
	 */
	public static final String ADMIN_APP_ID_DOCUMENT = "document";
	/**
	 * システム管理アプリケーション種別ID：帳票管理
	 */
	public static final String ADMIN_APP_ID_FORM = "form";
	/**
	 * システム管理アプリケーション種別ID：タスク管理
	 */
	public static final String ADMIN_APP_ID_TASK = "task";

	/* --- 画面種別 ------------------------------------------- */

	/**
	 * 画面種別ID：ログイン画面
	 */
	public static final String SCREEN_TYPE_LOGIN = "login";
	/**
	 * 画面種別ID：トップ画面
	 */
	public static final String SCREEN_TYPE_TOP = "top";

	/* --- ステータスタイプ種別 ------------------------------------------- */

	/**
	 * 通過条件不要
	 */
	public static final int THROUGH_APPROVE_NONE = 0;

	/**
	 * 通過条件：全員
	 */
	public static final int THROUGH_APPROVE_ALL = 2;
	/**
	 * 通過条件：一人
	 */
	public static final int THROUGH_APPROVE_ONE = 3;


	/* --- ステータスタイプ種別 ver4 ------------------------------------------- */

	/**
	 * ステータスタイプ種別：なし
	 */
	public static final int STATUS_TYPE_KIND_ID_NONE				 = 0;
	/**
	 * ステータスタイプ種別：編集中
	 */
	public static final int STATUS_TYPE_KIND_ID_EDITTING			 = -13001;
	/**
	 * ステータスタイプ種別：承認依頼中
	 */
	public static final int STATUS_TYPE_KIND_ID_APPROVE				 = -13002;

	/**
	 * ステータスタイプ種別：公開処理中
	 */
	public static final int STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC	 = -13003;
	/**
	 * ステータスタイプ種別：公開済
	 */
	public static final int STATUS_TYPE_KIND_ID_PUBLIC				 = -13004;

	/**
	 * ステータスタイプ種別：未着手
	 */
	public static final int STATUS_TYPE_KIND_ID_NOT_STARTED			= -23001;
	/**
	 * ステータスタイプ種別：対応中
	 */
	public static final int STATUS_TYPE_KIND_ID_ONGOING				= -23002;
	/**
	 * ステータスタイプ種別：承認中
	 */
	public static final int STATUS_TYPE_KIND_ID_APPROVING			= -23003;
	/**
	 * ステータスタイプ種別：完了
	 */
	public static final int STATUS_TYPE_KIND_ID_DONE					= -23004;
	
	/**
	 * ベースイベントタイプ：承認依頼
	 */
	public static final int BASE_EVENT_TYPE_ID_REQ_APPROVE			 = -14001;

	/**
	 * ベースイベントタイプ：承認
	 */
	public static final int BASE_EVENT_TYPE_ID_APPROVAL				 = -14002;

	/**
	 * ベースイベントタイプ：公開
	 */
	public static final int BASE_EVENT_TYPE_ID_PUBLIC				 = -14003;

	/**
	 * ベースイベントタイプ：承認依頼取消
	 */
	public static final int BASE_EVENT_TYPE_ID_CANCEL_REQ_APPROVE	 = -14004;

	/**
	 * ベースイベントタイプ：差戻し
	 */
	public static final int BASE_EVENT_TYPE_ID_SEND_BACK			 = -14005;

	/**
	 * ベースイベントタイプ：取戻し
	 */
	public static final int BASE_EVENT_TYPE_ID_TAKE_BACK			 = -14006;

	/**
	 * ベースイベントタイプ：公開取消
	 */
	public static final int BASE_EVENT_TYPE_ID_PUBLIC_CANCEL		 = -14007;


	/**
	 * メール種別：承認依頼通知
	 */
	public static final int MAIL_TYPE_ID_REQUEST_APPROVE	= -12001;

	/**
	 * メール種別：承認通知
	 */
	public static final int MAIL_TYPE_ID_APPROVE		 = -12002;

	/**
	 * メール種別：差戻し通知
	 */
	public static final int MAIL_TYPE_ID_SEND_BACK		 = -12003;

	/**
	 * メール種別：公開通知
	 */
	public static final int MAIL_TYPE_ID_PUBLIC			 = -12005;

	/**
	 * メール種別：公開取消通知
	 */
	public static final int MAIL_TYPE_ID_PUBLIC_CANCEL		= -12006;

	/**
	 * メール種別：有効期限切れドキュメント削除通知
	 */
	public static final int MAIL_TYPE_ID_DISPOSE_EXPIRED_DOCS		= -12007;

	/**
	 * メール種別：有効期限事前通知
	 */
	public static final int MAIL_TYPE_ID_EXPIRATION_DATE		= -12008;

	/**
	 * 管理者権限ID：汎用システム管理
	 */
	public static final String ADMIN_AUTH_ID_GENERAL = "general";
	/**
	 * 管理者権限ID：ドキュメントタイプ管理
	 */
	public static final String ADMIN_AUTH_ID_DOCUMENTTYPE = "documenttype";
	/**
	 * 管理者権限ID：カスタム属性管理
	 */
	public static final String ADMIN_AUTH_ID_ATTRIBUTE = "attribute";
	/**
	 * 管理者権限ID：ワークフロー管理
	 */
	public static final String ADMIN_AUTH_ID_WORKFLOW = "workflow";
	/**
	 * 管理者権限ID：フォーマット管理
	 */
	public static final String ADMIN_AUTH_ID_FORMAT = "format";
	/**
	 * 管理者権限ID：ユーザ管理
	 */
	public static final String ADMIN_AUTH_ID_USER = "user";
	/**
	 * 管理者権限ID：セキュリティ管理
	 */
	public static final String ADMIN_AUTH_ID_SECURITY = "security";
	/**
	 * 管理者権限ID：ワークスペース管理
	 */
	public static final String ADMIN_AUTH_ID_WORKSPACE = "workspace";
	/**
	 * 管理者権限ID：操作履歴管理
	 */
	public static final String ADMIN_AUTH_ID_OPEHIST = "opehist";
	/**
	 * 管理者権限ID：セキュリティエントリ管理
	 */
	public static final String ADMIN_AUTH_ID_SECURUTY_ENTRY = "securityEntry";
	/**
	 * 管理者権限ID：属性ツリービュー管理
	 */
	public static final String ADMIN_AUTH_ID_ATTRIUTE_TREE = "attributeTree";
	/**
	 * 管理者権限ID：帳票システム管理
	 */
	public static final String ADMIN_AUTH_ID_FORM = "form";
	/**
	 * 管理者権限ID：タスクシステム管理
	 */
	public static final String ADMIN_AUTH_ID_TASK = "task";

	/* --- ガード条件 ------------------------------------------- */

	/**
	 * ガード条件ID：なし
	 */
	public static final long GUARD_COND_ID_NOHTING = -15001;

	/**
	 * ガード条件ID：最後の承認者ではない
	 */
	public static final long GUARD_COND_ID_FINAL_APPROVED_IS_NOT = -15002;

	/**
	 * ガード条件ID：PDF変換対象
	 */
	public static final long GUARD_COND_ID_PDF_CONVERSION = -15003;

	/**
	 * ガード条件ID：PDF変換対象外
	 */
	public static final long GUARD_COND_ID_NON_PDF_CONVERSION = -15004;

	/**
	 * ガード条件ID：実行者が現ステータスの承認者
	 */
	public static final long GUARD_COND_ID_CURRENT_APPROVAL_CLIENT = -15005;

	/**
	 * ガード条件ID：実行者が現ステータスの承認依頼者
	 */
	public static final long GUARD_COND_ID_CURRENT_APPROVAL_REQUEST = -15006;

	/* --- 承認者表示の「上長のみ表示」デフォルト設定 --------------------- */

	/**
	 * 「上長のみ表示」デフォルト設定：上長のみ表示
	 */
	public static final String DEFAULT_SHOW_APPROVER_BOSS_ONLY_ON = "on";

	/* --- アプリケーション追加アクセスロール ----------------------------- */

	/**
	 * 常時読取
	 */
	public static final int ACCESS_ROLE_ALWAYS_READ = 500;

	/* --- 属性ツリーの分類対象 ----------------------------- */

	/**
	 * 分類対象：フォルダ
	 */
	public static final int CLASSIFY_TARGET_FOLDER = 0;

	/**
	 * 分類対象：ドキュメント
	 */
	public static final int CLASSIFY_TARGET_DOCUMENT = 1;

	/* --- 署名・暗号化状態 ----------------------------- */
	/**
	 * 署名・暗号化状態：署名・暗号化していない
	 */
	public static final int  SIGNENCR_KIND_NOT_SIGNENCR			= 0;
	/**
	 * 署名・暗号化状態：署名・暗号化済み（処理成功）
	 */
	public static final int  SIGNENCR_KIND_SIGNENCR				= 1;
	/**
	 * 署名・暗号化状態：署名・暗号化処理中
	 */
	public static final int  SIGNENCR_KIND_PROCESSING_SIGNENCR	= 2;
	/**
	 * 署名・暗号化状態：署名・暗号化処理に失敗
	 */
	public static final int  SIGNENCR_KIND_FAILED				= 3;

	/* --- タグ付与対象一覧種別 ----------------------------- */
	/**
	 * タグ付与対象一覧種別：タグ付与対象一覧以外
	 */
	public static final int TAG_LIST_KIND_NOT_TAG_LIST = 0;

	/**
	 * タグ付与対象一覧種別：ルートタグ
	 */
	public static final int TAG_LIST_KIND_ROOT_TAG = 1;

	/**
	 * タグ付与対象一覧種別：タグ配下のタグ
	 */
	public static final int TAG_LIST_KIND_TAG_UNDER_TAG = 2;

	/**
	 * タグ付与対象一覧種別：タグ配下のフォルダ
	 */
	public static final int TAG_LIST_KIND_TAG_UNDER_FOLDER = 3;

	/* --- 全体：フラグ --- */
	public static final int FLAG_ON  = 1;
	public static final int FLAG_OFF = 0;

	/**
	 * リンク更新タイミング
	 */
	public static final int LINK_UPDATE_TIMING_MANUAL  = 0;
	public static final int LINK_UPDATE_TIMING_AUTO = 1;

	/**
	 * OCR設定有無：無
	 */
	public static final int OCR_SETTING_NONE = 0;
	/**
	 * OCR設定有無：有
	 */
	public static final int OCR_SETTING_THERE = 1;

	/**
	 * OCR処理ステータス：未設定
	 */
	public static final int OCR_PROC_STATUS_NONE = -1;
	/**
	 * OCR処理ステータス：処理待ち
	 */
	public static final int OCR_PROC_STATUS_PROCESS_WAIT = 0;
	/**
	 * OCR処理ステータス：処理中
	 */
	public static final int OCR_PROC_STATUS_PROCESSING = 1;
	/**
	 * OCR処理ステータス：処理完了
	 */
	public static final int OCR_PROC_STATUS_PROCESS_COMPLETE = 2;

	/**
	 * OCR結果ステータス：未設定
	 */
	public static final int OCR_RESULT_STATUS_NONE = -1;
	/**
	 * OCR結果ステータス：成功
	 */
	public static final int OCR_RESULT_STATUS_SUCCESS = 1;
	/**
	 * OCR結果ステータス：失敗
	 */
	public static final int OCR_RESULT_STATUS_FAILURE = 9;

	/**
	 * PDF変換ステータス：処理なし
	 */
	public static final int PDF_CONVERSION_STATUS_NONE = -1;
	/**
	 * PDF変換ステータス：変換中
	 */
	public static final int PDF_CONVERSION_STATUS_PROCESSING = 1;
	/**
	 * PDF変換ステータス：変換完了（原本ファイルと乖離なし）
	 */
	public static final int PDF_CONVERSION_STATUS_PROCESS_COMPLETE_SAME_ORIGINAL = 2;
	/**
	 * PDF変換ステータス：変換完了（原本ファイルと乖離あり）
	 */
	public static final int PDF_CONVERSION_STATUS_PROCESS_COMPLETE_NOT_SAME_ORIGINAL = 3;
	/**
	 * PDF変換ステータス：変換失敗
	 */
	public static final int PDF_CONVERSION_STATUS_PROCESS_FAILURE = 9;

	/* --- 操作履歴：操作種別 ------------------------------------- */

	/**
	 * 操作種別(システム管理用)：公開通知先エントリー登録(ユーザ)
	 */
	public static final String REGIST_USER_ENTRY_FOR_PUBLIC = "1103";

	/**
	 * 操作種別(システム管理用)：公開通知先エントリー登録(グループ)
	 */
	public static final String REGIST_GROUP_ENTRY_FOR_PUBLIC = "1104";

	/**
	 * 操作種別(システム管理用)：公開通知先エントリー登録(ロール)
	 */
	public static final String REGIST_ROLE_ENTRY_FOR_PUBLIC = "1105";

	/**
	 * 操作種別(システム管理用)：公開通知先エントリー登録(複合グループ)
	 */
	public static final String REGIST_COMP_ENTRY_FOR_PUBLIC = "1106";

	/**
	 * 操作種別(システム管理用)：公開通知先エントリー削除(ユーザ)
	 */
	public static final String DELETE_USER_ENTRY_FOR_PUBLIC = "1107";

	/**
	 * 操作種別(システム管理用)：公開通知先エントリー削除(グループ)
	 */
	public static final String DELETE_GROUP_ENTRY_FOR_PUBLIC = "1108";

	/**
	 * 操作種別(システム管理用)：公開通知先エントリー削除(ロール)
	 */
	public static final String DELETE_ROLE_ENTRY_FOR_PUBLIC = "1109";

	/**
	 * 操作種別(システム管理用)：公開通知先エントリー削除(複合グループ)
	 */
	public static final String DELETE_COMP_ENTRY_FOR_PUBLIC = "1110";

	/**
	 * 操作種別(システム管理用)：表示列更新
	 */
	public static final String FORM_LIST_COLUMN_EDIT = "1134";

	/**
	 * 操作種別(システム管理用)：クラス属性引継ぎ・関連付け設定
	 */
	public static final String INHERITANCE_AND_RELATION_CLASS_ATTRIBUTE = "1137";

	/**
	 * 操作種別(ドキュメント管理用)：公開ファイル結合
	 */

	public static final String UNITING_PUBLIC_FILES = "2036";

	/**
	 * 操作種別(ドキュメント管理用)：ブランチコピー
	 */
	public static final String BRUNCH_COPY = "2037";

	/**
	 * 操作種別(ドキュメント管理用)：手動更新ドキュメントリンク作成
	 */
	public static final String DOCLINK_CREATE = "2038";

	/**
	 * 操作種別(ドキュメント管理用)：ドキュメントリンク移動
	 */
	public static final String DOCLINK_MOVE = "2039";

	/**
	 * 操作種別(ドキュメント管理用)：ドキュメントリンク削除
	 */
	public static final String DOCLINK_DELETE = "2040";

	/**
	 * 操作種別(ドキュメント管理用)：公開ファイルセキュリティ設定
	 */
	public static final String SECSETTING_PUBLIC_FILES = "2041";

	/**
	 * 操作種別(ドキュメント管理用)：文書属性一括更新 TODO 正しい番号を入れる
	 */
	public static final String DOCUMENT_DOCATTR_UPDATE = "2201";

	/* --------------------------------------------*/
	/* [08/01/27 added by ik.]
	 * 操作種別(ドキュメント管理用)：フォルダツリー複製 -> ドキュメント含む（手動更新リンクとして複製）
	 */
	public static final String FOLDER_TREE_COPY_WITH_DOC_LINK = "2042";
	/* --------------------------------------------*/

	/**
	 * 操作種別(ドキュメント管理用)：タグ新規登録
	 */
	public static final String CREATE_TAG = "2043";

	/**
	 * 操作種別(ドキュメント管理用)：タグ割当(タグを割当)
	 */
	public static final String ADD_TAG_A = "2044";

	/**
	 * 操作種別(ドキュメント管理用)：タグ割当(選択ドキュメント/フォルダ/タグに割当)
	 */
	public static final String ADD_TAG_B = "2045";

	/**
	 * 操作種別(ドキュメント管理用)：ドキュメント新規登録（外部コマンド）
	 */
	public static final String CREATE_DOCUMENT_EXCOMMAND = "2046";

	/**
	 * 操作種別(ドキュメント管理用)：タグ割当(外部コマンド)
	 */
	public static final String ADD_TAG_EXCOMMAND = "2047";

	/**
	 * 操作種別(ドキュメント管理用)：署名・暗号化
	 */
	public static final String SIGN_AND_ENCR = "2048";

	/**
	 * 操作種別(ドキュメント管理用)：一覧エクスポート
	 */
	public static final String EXPORT_LIST = "2049";

	/**
	 * 操作種別(ドキュメント管理用)：タグ新規登録(外部コマンド)
	 */
	public static final String CREATE_TAG_EXCOMMAND = "2050";

	/**
	 * 操作種別(ドキュメント管理用)：公開ファイル比較
	 */
	public static final String COMPARING_PUBLIC_FILES = "2051";

	/**
	 * 操作種別(ドキュメント管理用)：ZIPアップロード
	 */
	public static final String ZIP_UPLOAD = "2052";

	/**
	 * 操作種別(ドキュメント管理用)：ドキュメントリンク最新化
	 */
	public static final String DOCLINK_UPDATE = "2053";

	/**
	 * 操作種別(ドキュメント管理用)：CSVダウンロード
	 */
	public static final String DOWNLOAD_CSV = "2054";

	/**
	 * 操作種別(ドキュメント管理用)：リンク設定更新
	 */
	public static final String UPDATE_LINK_SETTING = "2107";

	/**
	 * 操作種別(ドキュメント管理用)：公開時更新ドキュメントリンク作成
	 */
	public static final String DOCLINK_CREATE_AUTO = "2108";

	/**
	 * 操作種別(ドキュメント管理用)：フォルダツリー複製 -> ドキュメント含む（公開時リンクとして複製）
	 */
	public static final String FOLDER_TREE_COPY_WITH_DOC_LINK_AUTO = "2109";

	/**
	 * 操作種別(ドキュメント管理用)：OCR処理設定
	 */
	public static final String OCR_SETTING = "2110";

	/**
	 * 操作種別(ドキュメント管理用)：OCR処理実行
	 */
	public static final String OCR_EXECUTE = "2111";

	/**
	 * 操作種別(ドキュメント管理用)：OCR処理取消
	 */
	public static final String OCR_CANCEL = "2112";

	/**
	 * 操作種別(ドキュメント管理用)：公開取消
	 */
	public static final String PUBLIC_CANCEL = "2113";

	/**
	 * 操作種別(ドキュメント管理用)：チェックイン(承認中)
	 */
	public static final String CHECKIN_APPROVER = "2114";

	/**
	 * 操作種別(ドキュメント管理用)：公開通知先変更
	 */
	public static final String CHENGE_PUBLISHER = "2115";

	/**
	 * 操作種別(ドキュメント管理用)：公開通知先テンプレート登録
	 */
	public static final String CREATE_PUBLISH_TEMPLATE = "2116";

	/**
	 * 操作種別(ドキュメント管理用)：公開通知先テンプレート更新
	 */
	public static final String UPDATE_PUBLISH_TEMPLATE = "2117";

	/**
	 * 操作種別(ドキュメント管理用)：公開通知先テンプレート更新
	 */
	public static final String DELETE_PUBLISH_TEMPLATE = "2118";
	/* --- 操作履歴：操作対象情報 ---------------------------- */

	/**
	 * 操作対象情報：ブランチコピー元
	 */
	public static final String BRUNCH_COPY_ORIGIN = "33";

	/**
	 * 操作対象情報：リンク元
	 */
	public static final String LINK_ORIGIN = "34";

	/**
	 * 操作対象情報：設定対象
	 */
	public static final String TARGET_TO_SETTING = "35";

	/**
	 * 操作対象情報：公開ファイル結合情報
	 */
	public static final String INFO_UNITING_PUBLIC_FILES = "36";

	/**
	 * 操作対象情報：公開ファイルセキュリティ設定情報
	 */
	public static final String INFO_SECSETTING_PUBLIC_FILES = "37";

	/**
	 * 操作対象情報：割当タグ
	 */
	public static final String ADDED_TAG = "38";

	/**
	 * 操作対象情報：割当対象
	 */
	public static final String TARGET_TO_ADD = "39";

	/**
	 * 操作対象情報：署名・暗号化対象
	 */
	public static final String TARGET_TO_SIGN_AND_ENCR = "40";

	/**
	 * 操作対象情報：解除対象
	 */
	public static final String TARGET_TO_REMOVE = "41";

	/**
	 * 操作対象情報：エクスポート対象
	 */
	public static final String TARGET_TO_EXPORT = "42";

	/**
	 * 操作対象情報：リンク先
	 */
	public static final String LINK_SOURCE = "43";

	/**
	 * 操作対象情報：複製対象
	 */
	public static final String TARGET_COPY = "44";

	/**
	 * 操作対象情報：結合対象
	 */
	public static final String TARGET_TO_UNITE = "45";

	/**
	 * 操作対象情報：CAD端末
	 */
	public static final String CAD_TERMINAL = "99";

	/**
	 * 操作対象種別：テンプレート
	 */
	public static final String TEMPLATE = "20";

	/* --- CSVファイルダウンロード ---------------------------------------- */
	/**
	 * 画面から引き渡されるXML文字列のエンコーディング
	 */
	public static final String CSV_XMLENCODING = "UTF-8";
	/**
	 * ダブルクォーテーション文字エスケープ
	 */
	public static final String CSV_ESCDQUOTATION = "\"";
	/**
	 * CSV列デリミタ(カンマ)
	 */
	public static final String CSV_COLDELIMITER = ",";
	/**
	 * 文字コード:UTF-8
	 */
	public static final String CSV_OUTPUT_UTF8 = "UTF-8";
	/**
	 * UTF-8(BOMあり)で出力時の指定キー(config_doc.properties  CSV_DOWNLOAD_CHARSET)
	 */
	public static final String CSV_OUTPUT_UTF8_BOM = "UTF-8_BOM";


	/* --- 公開通知先 エントリータイプ ---------------------------------------- */

	/**
	 * 公開通知送信先	エントリータイプID（ユーザ）
	 */
	public static final String ENTRY_TYPE_USER				= "1";

	/**
	 * 公開通知送信先	エントリータイプID（グループ）
	 */
	public static final String ENTRY_TYPE_GROUP				= "2";

	/**
	 * 公開通知送信先	エントリータイプID（ロール）
	 */
	public static final String ENTRY_TYPE_ROLE				= "3";

	/**
	 * 公開通知送信先	エントリータイプID（複合グループ）
	 */
	public static final String ENTRY_TYPE_COMP_GROUP		= "4";

	/**
	 * 公開通知送信先	エントリータイプID（ユーザ定義グループ）
	 */
	public static final String ENTRY_TYPE_USER_DEF_GROUP	= "5";

	/**
	 * 公開通知送信先	エントリータイプID（システム処理）
	 */
	public static final String ENTRY_TYPE_SYS_FUNC			= "6";


	/* --- ワークスペース責任者 エントリータイプ ---------------------------------------- */

	/**
	 * ワークスペース責任者	エントリータイプID（ユーザ）
	 */
	public static final int WS_ADMIN_ENTRY_TYPE_USER		= 1;

	/**
	 * ワークスペース責任者	エントリータイプID（グループ）
	 */
	public static final int WS_ADMIN_ENTRY_TYPE_GROUP		= 2;

	/**
	 * ワークスペース責任者	エントリータイプID（ロール）
	 */
	public static final int WS_ADMIN_ENTRY_TYPE_ROLE		= 3;

	/**
	 * ワークスペース責任者	エントリータイプID（複合グループ）
	 */
	public static final int WS_ADMIN_ENTRY_TYPE_COMP_GROUP	= 4;


	/* --- メール通知 承認依頼通知/公開通知タイミング ---------------------------------------- */

	/**
	 * メール通知タイミング 即時
	 */
	public static final int MAILNOTICE_TIMING_IMMEDIATE = 0;

	/**
	 * メール通知タイミング 定時
	 */
	public static final int MAILNOTICE_TIMING_SCHEDULED = 1;

	/**
	 * メール通知タイミング 無し
	 */
	public static final int MAILNOTICE_TIMING_OFF		= 3;

	/**
	 * メール通知タイミング 即時(文字列)
	 */
	public static final String MAILNOTICE_TIMING_IMMEDIATE_STR = "immediate";

	/**
	 * メール通知タイミング 定時(文字列)
	 */
	public static final String MAILNOTICE_TIMING_SCHEDULED_STR = "scheduled";

	/**
	 * メール通知タイミング 無し(文字列)
	 */
	public static final String MAILNOTICE_TIMING_OFF_STR		= "off";

	/* --- 上長承認 承認不要/要承認  ---------------------------------------- */

	/**
	 * 上長承認 要承認
	 */
	public static final String BOSSAPPROVAL_NECESSARY = "necessary";

	/**
	 * 上長承認 承認不要
	 */
	public static final String BOSSAPPROVAL_UNNECESSARY = "unnecessary";

	/* --- パラメータ取得キー ---------------------------------------- */

	/**
	 * 承認依頼情報ドメイン
	 */
	public static final String PARAM_APPROVAL_REQINFO = "ApprovalReqInfo";

	/**
	 * 処理種別：承認参照
	 * <ul>
	 * <li>
	 * ドキュメント管理ではベースイベントタイプのenabledメソッドでアサイン先予定します。<br/>
	 * ST遷移予測でもベースイベントタイプのenabledメソッドが呼ばれるため<br/>
	 * イベント実行時とST遷移予測時の処理を分ける必要があるため、<br/>
	 * リクエストパラメータのprocessTypeに本定数値があればST遷移予測用の処理を行うための<br/>
	 * フラグの意味を持ちます。
	 * </li>
	 * </ul>
	 */
	public static final String PARAM_KEY_REFER_TO_APPROVAL = "processType";
	public static final String PARAM_VALUE_REFER_TO_APPROVAL = "referToApproval";
	public static final String PARAM_VALUE_REFER_TO_CANCEL_APPROVAL = "referToCancelApproval";
	public static final String PARAM_VALUE_REFER_TO_PUBLIC_CANCEL = "referToPublicCancel";

	/**
	 * 承認不要WFの公開機能
	 * <ul>
	 * <li>
	 * 承認不要WFの公開機能は、「承認」ベースイベントタイプで実行します。<br/>
	 * BaseEvtApprovalPlugInImpl にて処理を分けるためのフラグです。
	 * </li>
	 * </ul>
	 */
	public static final String PARAM_KEY_IMMEDIATE_PUBLIC_NAME = "immediatePublicFlag";

	/* --- フラグ ------------------------------------------- */

	/**
	 * 無効フラグ：オン
	 */
	public static final int INVALID_FLAG_ON = 1;

	/**
	 * 無効フラグ：オフ
	 */
	public static final int INVALID_FLAG_OFF = 0;

	/**
	 * authorize()使用フラグ：オン(使用する)
	 */
	public static final int AUTHORIZE_METHOD_USE_FLAG_ON = 1;

	/**
	 * authorize()使用フラグ：オフ(使用しない)
	 */
	public static final int AUTHORIZE_METHOD_USE_FLAG_OFF = 0;

	/**
	 * 遷移先ステータスタイプ予測フラグ：オン(予測する)
	 */
	public static final int FORECAST_NEXT_STATUSTYPE_FLAG_ON = 1;

	/**
	 * authorize()使用フラグ：オフ(予測しない：ドキュメント管理用WFの簡易処理)
	 */
	public static final int FORECAST_NEXT_STATUSTYPE_FLAG_OFF = 0;

	/* --- 検索条件追加プラグイン ------------------------------------------- */
	/**
	 * 検索条件追加プラグイン埋め込み種別をスレッドローカルに保存するためのキー
	 */
	public static final String SEARCH_CONDITION_EMBEDDED_TYPE = "eimDocSearchType";

	/* --- UIコントロール属性データ型名称 ------------------------------------------- */
	/**
	 * UIコントロール属性データ型名称：数値型
	 */
	public static final String UICONTROL_VALUE_TYPE_NAME_LONG = "long";

	/**
	 * UIコントロール属性データ型名称：文字列型
	 */
	public static final String UICONTROL_VALUE_TYPE_NAME_STRING = "string";

	/**
	 * UIコントロール属性データ型名称：日付型
	 */
	public static final String UICONTROL_VALUE_TYPE_NAME_DATE = "date";

	/**
	 * UIコントロール属性データ型名称：テキスト型
	 */
	public static final String UICONTROL_VALUE_TYPE_NAME_TEXT = "text";

	/**
	 * UIコントロール属性データ型名称：実数型
	 */
	public static final String UICONTROL_VALUE_TYPE_NAME_DOUBLE = "double";

	/**
	 * UIコントロール属性データ型名称：オブジェクト型
	 */
	public static final String UICONTROL_VALUE_TYPE_NAME_OBJECT = "object";

	/**
	 * UIコントロール属性データ型名称：ユーザ型
	 */
	public static final String UICONTROL_VALUE_TYPE_NAME_USER = "user";

	/**
	 * UIコントロール属性データ型名称：コード型
	 */
	public static final String UICONTROL_VALUE_TYPE_NAME_CODE = "code";

	/* --- UIコントロール多重度 ------------------------------------------- */
	/**
	 * UIコントロール多重度：なし
	 */
	public static final String UICONTROL_SINGLE = "single";

	/**
	 * UIコントロール多重度：あり
	 */
	public static final String UICONTROL_MULTIPLE = "multiple";

	/* --- 初期値 ------------------------------------------- */
	/**
	 *ユーザ型初期値：ログインユーザ
	 */
	public static final String USER_INIT_ATTR_LOGIN_USER = "login_user";

	/* --- 共通 ------------------------------------------- */

	/**
	 * 区切り文字：アンダースコア
	 */
	public static final String DELIMITER_UNDER_SCORE = "_";

	/* --- 帳票システム管理（複製設定操作履歴） ------------------------------------------- */

	/**
	 * 操作履歴(複製設定)
	 */
	public static final String NEWCOPY_CLASS_ATTRIBUTE = "1133";

	/* --- 表示列  ---------------------------------------- */
	/**
	 * 表示列:ID
	 */
	public static final String FORM_LIST_COLUMN_ID = "name";

	/**
	 * 表示列:タイトル
	 */
	public static final String FORM_LIST_COLUMN_TITLE = "title";

	/**
	 * 表示列:ステータス
	 */
	public static final String FORM_LIST_COLUMN_STATUS = "status.type.name";

	/**
	 * 表示列:作成者
	 */
	public static final String FORM_LIST_COLUMN_CREATION_USER = "creationUser.name";

	/**
	 * 表示列:作成日時
	 */
	public static final String FORM_LIST_COLUMN_CREATION_DATE = "creationDate";

	/**
	 * 表示列:更新者
	 */
	public static final String FORM_LIST_COLUMN_MODIFICATION_USER = "modificationUser.name";

	/**
	 * 表示列:更新日時
	 */
	public static final String FORM_LIST_COLUMN_MODIFICATION_DATE = "modificationDate";

	/* --- 紙文書電子化オプション  ---------------------------------------- */
	/**
	 * スキャン用表紙PDFのQRコードに埋め込むヘッダ
	 */
	public static final String QR_CODE_EMBEDDED_HEADER = "EIM-PDF-HEADER:";

	/**
	 * QRコード：イメージサイズ
	 * 誤り訂正レベルHで、PDF_AUTO_REGIST_QR_CODE_HEADERの文字数とID32文字を含められるサイズにすること
	 */
	public static final int QR_CODE_IMAGE_SIZE = 41;

	/**
	 * 読込PDFナンバリング桁数
	 */
	public static final int SCAN_SEQ_MAX_SCALE = 4;

	/**
	 * QRコード埋め込みID最大桁数
	 */
	public static final int QR_CODE_OBJID_MAX_SCALE = 32;

	/**
	 * ドキュメントサービスファイルアップロード方法（IO）
	 */
	public static final String FILE_UPLOAD_RULE_IO = "IO";

	/**
	 * ドキュメントサービスファイルアップロード方法（FTP）
	 */
	public static final String FILE_UPLOAD_RULE_FTP = "FTP";

	/* --- 属性一括登録 ------------------------------------------- */

	/**
	 * 出力処理件数単位
	 */
	public static final int PROCESS_COUNT = 100;



}