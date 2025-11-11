package common.enumeration;

import java.util.HashMap;
import java.util.Map;

import common.enumeration.HGPScanErrorCodeEnum;

/**
 * HGPScanの
 * 
 * @since Ver6.0
 */
/** エラーコード列挙型 */
public enum HGPScanErrorCodeEnum {
	/** 未実行 */
	UNEXECUTE(-9999, ""),
	/** 予期しない(存在しない)エラー */
	FAILED(-1,		"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.FAILED"),
	/** 異常なし */
	NONE(0,""),
	/** HGPScanアプリケーションエラー */
	/** サポートされていない画像形式 */
	SUPPORT_IMAGE(		1,		"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.SUPPORT.IMAGE"),
	/** PDFが開かれているか権限がない */
	OPEN_FILE(			2,		"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.OPEN.FILE"),
	/** 処理中に対象ファイルが移動または削除された */
	DATA_NOT_FOUND(		3,		"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.DATA.NOT.FOUND"),
	/** 画像データ内に解像度情報がない */
	DPI_DETAIL(			4,		"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.DPI.DETAIL"),
	/** 読み込めないDPF */
	SUPPORT_PDF(		11,		"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.SUPPORT.PDF"),
	/** 出力先がない、または権限がない */
	FINDING_OUTPUT(		12,		"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.FINDING.OUTPUT"),
	/** ファイルセキュリティによりPDFが変換できない */
	OPEN_PDF(			13,		"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.OPEN.PDF"),
	/** ファイルセキュリティによりMS文書ファイルが開けない */
	OPEN_DOC(			14,		"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.OPEN.DOC"),
	/** ファイルの作成ができない */
	CREATE_FILE(		15,		"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.CREATE.FILE"),
	/** XDW変換に失敗(タイムアウトを含む) */
	CONVERT_XDW(		16,		"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.CONVERT.XDW"),
	/** XDW変換に失敗 */
	CONVERT_XDW2(		17,		"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.CONVERT.XDW2"),
	/** BMP変換に失敗しました */
	CONVERT_BMP(		18,		"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.CONVERT.BMP"),
	/** ファイル結合に失敗 */
	COMBINE_FILE(		19,		"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.COMBINE.FILE"),
	/** 外部指定データ形式の問題で変換に失敗 */
	EX_FILE_FORMAT(		20,		"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.EX.FILE.FORMAT"),
	/** 外部指定データの変換に失敗 */
	EX_FILE_CONVERT(	21,		"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.EX.FILE.CONVERT"),
	/** ファイルの分割に失敗 */
	FILE_SPLIT(			23,		"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.FILE.SPLIT"),
	/** CADデータの変換に失敗 */
	CONVERT_CAD(		24,		"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.CONVERT.CAD"),
	/** 結合できないPDF */
	CONBINE_PDF(		25,		"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.CONBINE.PDF"),
	/** 外部アプリ起動に失敗 */
	AFTER_EXEC_EX_APP(	30,		"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.AFTER.EXEC.EX.APP"),
	/** CSV情報ファイルが不正 */
	CSV_FORMAT(			31,		"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.CSV.FORMAT"),
	/** 日付印の作成に失敗 */
	CREATE_DATE_STAMP(	32,		"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.CREATE.DATE.STAMP"),
	/** フォントの埋め込みに失敗 */
	EMBED_FONT(			33,		"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.EMBED.FONT"),
	/** PDFの復号に失敗 */
	DECRYPT_PDF(		34,		"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.DECRYPT.PDF"),
	/** 変換対象外のファイル */
	EXEMPT_FILE(		36,		"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.EXEMPT.FILE"),
	/** メール送信エラー */
	SENDING_MAIL(		40,		"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.SENDING.MAIL"),
	/** コードワードがない */
	CODE_WORD(			50,		"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.CODE.WORD"),
	/** 存在しないジョブ */
	JOB_NAME(			110,	"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.JOB.NAME"),
	/** PDFの電子署名に失敗 */
	PDF_SIGNATURE(		121,	"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.PDF.SIGNATURE"),
	/** アマノタイムスタンプサービス設定に失敗 */
	TIMESTAMP_SERVICE(	122,	"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.TIMESTAMP.SERVICE"),
	/** 長期署名の設定に失敗 */
	LONG_SIGNATURE(		123,	"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.LONG.SIGNATUER"),
	/** タイムシールの処理に失敗 */
	TIME_SEAL(			130,	"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.TIME.SEAL"),

	/** コマンド実行エラー */
	/** インストールされていません */
	NOT_INSTALL(		5001,	"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.NOT.INSTALL"),
	/** 監視状態ではありません */
	NOT_MONITORING(		5002,	"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.NOT.MONITORING"),
	/** 別の変換処理を実行中です */
	PROCCESSING_OTHER(	5003,	"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.PROCCESSING.OTHER"),
	/** コマンドを重複実行できません */
	COMMAND_DUPLICATION(5010,	"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.COMMAND.DUPLICATION"),
	/** (内部)コマンド指示エラー */
	COMMAND_INDICATION(	5011,	"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.COMMAND.INDICATION"), 
	/** (内部)実行情報取得に失敗 */
	FAILED_EXEC_DETAIL(	5012,	"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.FAILED.EXEC.DETAIL"), 
	/** (内部)コマンド実行に失敗 */
	COMMAND_EXEC(		5013,	"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.COMMAND.EXEC"), 
	/** (内部)実行時間タイムアウト */
	TIMEOUT(			5020,	"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.TIMEOUT"), 
	/** (内部)予期しないエラー */
	UNEXPECTED(			5050,	"EIM.ERROR.LOGIC.FAILED.PDF.HGPSCAN.UNEXPECTED"),
	;
	
	/** staticフィールド */
	/** ERROR 逆引きマップ */
	private static final Map<Integer, HGPScanErrorCodeEnum> CODE2ERROR = new HashMap<Integer, HGPScanErrorCodeEnum>();
	/** 逆引きマップの内容をstaticイニシャライズ */
	static {
		for (HGPScanErrorCodeEnum errorEnum : HGPScanErrorCodeEnum.values()) {
			CODE2ERROR.put(errorEnum.code, errorEnum);
		}
	}

	/**
	 * コードからERRORの逆引きを行う
	 * 
	 * @param code コード値
	 * @return 対応するERROR,対応するエラーコードがない場合、存在しないコード
	 */
	public static HGPScanErrorCodeEnum codeOf(int code) {
		if (!CODE2ERROR.containsKey(code)) {
			return  HGPScanErrorCodeEnum.FAILED;
		}
		return CODE2ERROR.get(code);
	}

	/** インスタンスフィールド */
	/** エラーコード値 */
	public int code;
	/** エラーメッセージキー */
	public String key;

	/** コンストラクタ */
	private HGPScanErrorCodeEnum(int code, String key) {
		this.code = code;
		this.key = key;
	}
	
}