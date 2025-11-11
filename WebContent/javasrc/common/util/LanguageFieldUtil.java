package common.util;

import jakarta.servlet.http.HttpServletRequest;

import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.EIMUtils;
import eim.util.StringUtils;

/**
 * 多言語フィールド　共通クラス
 * <P>
 * [機能]<BR>
 *		Flex側の多言語コンポーネント情報を
 * 		(common/component/EIMLanguageField.mxml)
 * 		JSPで編集する為のHelperクラス
 *
 * [備考]<BR>
 * 	EIMLanguageField.mxml以外の動作保障はしていません。<BR>
 *
 * [前提条件]<BR>
 * 	EIMLanguageField.mxml情報を取得すること<BR>
 *
 * @author tsumura
 * @version 1.0
 */
public class LanguageFieldUtil {

	/** 定数定義：言語種類の合計 */
	public static final String PARAM_OTHRE_CNT = "otherCnt";
	/** 定数定義：言語名称 */
	public static final String PARAM_OTHRE_NAME = "otherName";
	/** 定数定義：言語ID */
	public static final String PARAM_OTHRE_LID = "otherLId";

	/**
	 * 言語種類の合計の取得
	 *
	 * @param request
	 * @return otherCnt 言語種類の合計
	 */
	public static int getOtherCnt(HttpServletRequest request){

		int otherCnt = 0;
		if(request.getParameter(PARAM_OTHRE_CNT) != null && request.getParameter(PARAM_OTHRE_CNT).length() > 0){
			otherCnt = Integer.parseInt(request.getParameter(PARAM_OTHRE_CNT));
		}
		return otherCnt;
	}

	/**
	 * デフォルト名称の取得
	 *
	 * @param request
	 * @param cnt
	 * @return デフォルト名称
	 */
	public static String getDefName(HttpServletRequest request, int cnt){

		String defName = null;
		for(int i = 0; i < cnt; i++){
			if(!EIMUtils.getParameter(request, PARAM_OTHRE_LID+i).equals("JA")){
				continue;
			}
			defName = EIMUtils.getParameter(request, PARAM_OTHRE_NAME+i);
			break;
		}
		return defName;
	}

	/**
	 * ネームスペース付き定義名称の取得<br>
	 * ※namespaceがnullの場合は、request内に設定されたネームスペースを使用する
	 * ※nameがnullか空文字の場合は、request内に設定された日本語名称を定義名称として使用する
	 *
	 * @param sess EIMSession
	 * @param name 名称
	 * @param request リクエスト
	 * @param cnt reauestに設定された他言語名称数
	 * @return ネームスペース付きデフォルト名称
	 */
	public static String getDefName(EIMSession sess, String name, HttpServletRequest request, int cnt) {

		String defName = null;

		// nameが指定されていた場合
		if (!StringUtils.isBlank(name)) {

			// デフォルト名称として使用する
			defName = name;

		// nameが指定されていない場合
		} else {

			// 日本語名称をデフォルト名称として使用する
			for(int i = 0; i < cnt; i++){

				if(!EIMUtils.getParameter(request, PARAM_OTHRE_LID+i).equals("JA")) {
					continue;
				}

				defName = EIMUtils.getParameter(request, PARAM_OTHRE_NAME+i);

				break;
			}
		}

		//ネームスペースの付加
		String prmNamespace =  EIMUtils.getParameter(request, "namespace");
		if (prmNamespace != null && !prmNamespace.equals(""))
		{
			defName = prmNamespace + EIMConfig.get("NAMESPACE_DIVID_CHAR") + defName;
		}
		
		return defName;
	}

	/**
	 * 言語名称の取得
	 *
	 * @param request
	 * @param index
	 * @return 言語名称
	 */
	public static String getOtherName(HttpServletRequest request, int index){
		return EIMUtils.getParameter(request, PARAM_OTHRE_NAME+index);
	}

	/**
	 * 言語IDの取得
	 *
	 * @param request
	 * @param index
	 * @return 言語ID
	 */
	public static String getOtherLId(HttpServletRequest request, int index){
		return EIMUtils.getParameter(request, PARAM_OTHRE_LID+index);
	}

}
