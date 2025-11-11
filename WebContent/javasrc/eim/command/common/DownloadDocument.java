package eim.command.common;

import java.io.File;

import eim.bo.EIMAccessRole;
import eim.bo.EIMFile;
import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.bo.EIMResource;
import eim.bo.EIMVersion;
import eim.command.business.service.result.EIMCommandResultData;
import eim.command.common.util.DocumentManagementUtils;
import eim.net.EIMSession;
import eim.util.AccessUtils;
import eim.util.EIMConfig;
import eim.util.FileUtils;
import eim.util.SecurityUtils;
import eim.util.VersionUtils;

/**
 * ドキュメントダウンロード実行クラス
 *
 */
public class DownloadDocument {

	// セッション
	private EIMSession sess = null;

	// フォーマット
	private EIMFormat format = null;


	/** フォーマットコード */
	private final String FORMAT_CODE_DEFAULT	= "org";
	private final String FORMAT_CODE_PUBLIC		= "public";
	private final String FORMAT_CODE_SIGNED		= "signed";


	/**
	 * コンストラクタ
	 */
	public DownloadDocument(EIMSession _sess)
	{
		// セッションをセット
		this.sess = _sess;
	}

	/**
	 * コンストラクタ
	 */
	public DownloadDocument(EIMSession sess, EIMFormat format)
	{
		this.sess = sess;
		this.format = format;
	}

	/**
	 * ドキュメントダウンロードを行う
	 * @param paramFormat 引数にて指定されたフォーマットオプション
	 * @return
	 * @throws Exception
	 */
	public EIMCommandResultData doDownloadDocument(String paramFormat, EIMObject obj, EIMCommandResultData result, boolean isGet) throws Exception
	{
		// オブジェクトのチェックを行う
		if(!checkObj(result, obj, paramFormat, isGet))
		{
			// エラーであった場合
			return result;
		}

		// フォーマットの取得とセットを行う
		if(!checkFormat(paramFormat, result, obj))
		{
			//エラーであった場合
			return result;
		}

		// ファイルのダウンロードを行う
		result = downloadDocumentByObjAndFormat(obj, format, result);

		// ダウンロード結果の返却値がエラーであった場合
		if(result.getType() == EIMResource.getMessage(sess, "EIM.RESULT.TYPE.ERROR"))
		{
			return result;
		}
		else
		{
			// タイプに正常終了を設定する
			result.setType(EIMResource.getMessage(sess, "EIM.RESULT.TYPE.INFO"));
		}

		// アクセス履歴
		AccessUtils.createAccess(sess, obj, "EIM.ACCESS.TYPE.EXIF.DOWNLOAD");

		return result;
	}


	/**
	 * EIMオブジェクトをチェックする
	 * エラーの場合、レスポンス返却データクラスにデータを格納する
	 * @param result レスポンス返却データクラス
	 * @param obj チェック対象オブジェクト
	 * @return
	 * @throws Exception
	 */
	private Boolean checkObj(EIMCommandResultData result, EIMObject obj, String format, boolean isGet) throws Exception
	{
		// オブジェクトの取得に失敗した場合
		if(obj == null)
		{
			result.setTypeCodeMessage(EIMResource.getMessage(sess, "EIM.RESULT.TYPE.ERROR"),
										EIMResource.getMessage(sess, "EIM.ERROR.CODE.OJBECT.NO.EXIST"),
										EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.OJBECT.NO.EXIST"));
			return false;
		}

		// オブジェクトタイプが「ドキュメント」でない場合
		DocumentManagementUtils dmu = new DocumentManagementUtils(sess);
		if(!dmu.isDocumentTypes(obj.getType()))
		{
			result.setTypeCodeMessage(EIMResource.getMessage(sess, "EIM.RESULT.TYPE.ERROR"),
										EIMResource.getMessage(sess, "EIM.ERROR.CODE.OBJECT.ISNOT.DOCUMENT"),
										EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.OBJECT.ISNOT.DOCUMENT", new Object[]{Long.toString(obj.getId())}));
			return false;
		}

		// オブジェクトのロックチェックと参照権限チェックを行います
		if(!checkLockAndAuth(sess, obj, format, isGet))
		{
			result.setTypeCodeMessage(EIMResource.getMessage(sess, "EIM.RESULT.TYPE.ERROR"),
										EIMResource.getMessage(sess, "EIM.ERROR.CODE.NO.READ.AUTH"),
										EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.FILE.NO.READ.AUTH"));
			return false;
		}

		return true;
	}

	/**
	 * ロックオブジェクトの差し替えとオブジェクトの参照権限のチェックを行います
	 * getコマンド時、ロックされている場合はロックオブジェクトをダウンロードオブジェクトとします
	 *
	 */
	private boolean checkLockAndAuth(EIMSession sess, EIMObject obj, String format, boolean isGet) throws Exception{

		EIMVersion version = VersionUtils.getVersion(sess, obj);

		//オブジェクトが最新の場合
		if (obj.getRevision() == version.getMaxRevision()){

			if(obj.getRevision() > 0){
				EIMObject lockObj = version.getObjectByRev(obj.getRevision() - 1);
				//ロックユーザがいればロックがかかっている
				if(lockObj.getLockUser() != null){
					//コマンドがgetでフォーマットが公開ならロックユーザをダウンロードするために代入する
					if(isGet && format != null && format.equals(FORMAT_CODE_PUBLIC)){
						obj = lockObj;
					}
				}
			}
		}
		//参照権限チェック
		if(!SecurityUtils.authorized(sess, obj, sess.getUser(), EIMAccessRole.READ)){
			return false;
		}else{
			return true;
		}
	}


	/**
	 * EIMフォーマットをチェックする
	 * エラーの場合、レスポンス返却データクラスにデータを格納する
	 * @param paramFormat 引数にて指定されたフォーマットオプション
	 * @param result レスポンス返却データクラス
	 * @param obj ダウンロード対象オブジェクト
	 * @return
	 */
	private Boolean checkFormat(String paramFormat, EIMCommandResultData result, EIMObject obj) throws Exception
	{
		// フォーマットが指定されている場合
		if(paramFormat != null && !paramFormat.equals(""))
		{
			format = getParamFormat(paramFormat, obj);

			// 不正なフォーマットが指定されている場合
			if(format == null)
			{
				result.setTypeCodeMessage(EIMResource.getMessage(sess, "EIM.RESULT.TYPE.ERROR"),
											EIMResource.getMessage(sess, "EIM.ERROR.CODE.INVALID.OPTION"),
											EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.INVALID.OPTION"));
				return false;
			}
		}
		// フォーマットが指定されていない場合
		else
		{
			//まだフォーマットが設定されていない場合
			if(format == null)
			format = getDefaultFormat(obj);
		}

		// フォーマットの取得に失敗した場合
		if(format == null)
		{
			result.setTypeCodeMessage(EIMResource.getMessage(sess, "EIM.RESULT.TYPE.ERROR"),
										EIMResource.getMessage(sess, "EIM.ERROR.CODE.FORMAT.NO.EXIST"),
										EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.FORMAT.NO.EXIST"));
			return false;
		}

		return true;

	}

	/**
	 * 引数に指定されたフォーマットから、以下のEIMフォーマットを取得する
	 *  ・「署名/暗号化ファイル」
	 *  ・「公開ファイル」
	 *  ・「原本ファイル」
	 * @param paramFormat
	 * @return
	 * @throws Exception
	 */
	private EIMFormat getParamFormat(String paramFormat, EIMObject obj) throws Exception
	{
		EIMFormat fmt = null;

		// 「org(原本ファイル)」の場合
		if(paramFormat.equals(FORMAT_CODE_DEFAULT))
		{
			fmt = FileUtils.getDefaultFormat(sess, obj.getType());
		}
		// 「public(公開ファイル)」の場合
		else if(paramFormat.equals(FORMAT_CODE_PUBLIC))
		{
			fmt = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
		}
		// 「signed(署名/暗号化ファイル)」の場合
		else if(paramFormat.equals(FORMAT_CODE_SIGNED))
		{
			fmt = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_SIGNENCR"));
		}

		return fmt;

	}

	/**
	 * オブジェクトのステータスから、以下のEIMフォーマットを決定し、取得する
	 *  ・暗号化済みの場合					：「署名/暗号化ファイル」のEIMフォーマット
	 *  ・暗号化未実施、且つ公開済みの場合	：「公開ファイル」のEIMフォーマット
	 *  ・暗号化・公開ともに未実施の場合	：「原本ファイル」のEIMフォーマット
	 * @return
	 * @throws Exception
	 */
	private EIMFormat getDefaultFormat(EIMObject obj) throws Exception
	{
		EIMFormat fmt = null;

		// 属性「署名・暗号化状態」を取得
		long signed = 0;
		if(obj.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS")) != null)
		{
			signed = obj.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS")).getInt();
		}

		//「署名/暗号化ファイル」である場合
		if(signed == 1)
		{
			fmt = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_SIGNENCR"));
		}
		else
		{
			// 「公開ファイル」である場合
			fmt = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
			EIMFile publicFile = FileUtils.getFile(sess, obj, fmt);

			// 「公開ファイル」でない場合
			if(publicFile == null)
			{
				fmt = FileUtils.getDefaultFormat(sess, obj.getType());
			}
		}

		return fmt;
	}

	/**
	 * EIMObjectとEIMFormatから、ファイルのダウンロードを行う
	 *
	 * @param obj
	 * @param format
	 * @return
	 * @throws Exception
	 */
	public EIMCommandResultData downloadDocumentByObjAndFormat(EIMObject obj, EIMFormat format, EIMCommandResultData result) throws Exception
	{

		// EIMObj、EIMformatからEIMFileオブジェクトを取得する
		EIMFile fileObj = FileUtils.getFile(sess, obj, format);

		// 指定したフォーマットにファイルがチェックインされていない場合
		if(fileObj == null)
		{
			result.setTypeCodeMessage(EIMResource.getMessage(sess, "EIM.RESULT.TYPE.ERROR"),
										EIMResource.getMessage(sess, "EIM.ERROR.CODE.FILE.NOT.CHECKIN.TO.FORMAT"),
										EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.FILE.NOT.CHECKIN.TO.FORMAT"));
			return result;
		}

		// EIMFileオブジェクトから、ファイル実体のパスを取得する
		String path = fileObj.getDirectory().getPath() + FileUtils.getFileName(obj, fileObj);

		// ファイル実体のフルパスから、ファイルの実体を取得する
		File file = new File(path);

		// ファイルが取得できない場合
		if(file == null || !file.exists())
		{
			result.setTypeCodeMessage(EIMResource.getMessage(sess, "EIM.RESULT.TYPE.ERROR"),
										EIMResource.getMessage(sess, "EIM.ERROR.CODE.NOT.GET.FILE"),
										EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOT.GET.FILE"));
			return result;
		}

		//レスポンスに取得したファイルの情報をセットする
		result.setFileNameAndFile(fileObj.getName(), file);

		return result;
	}

}
