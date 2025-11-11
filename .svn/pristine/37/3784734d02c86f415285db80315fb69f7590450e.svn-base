package common.util;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXParseException;

import eim.bo.EIMException;
import eim.bo.EIMResource;
import eim.bo.EIMStatusType;
import eim.bo.EIMWorkFlow;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import jp.co.ctc_g.eim.framework2.business.domain.entity.OtherNameDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.StatusTypeDomain;
import jp.co.ctc_g.eim.framework2.business.service.StatusTypeService;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;

/**
 *
 * PDF署名バッチ(IOWebDocとHGPScan)用 クラス
 * 署名印のステータスタイプ欄の表示名をXMLから取得する
 *
 */
public class SignatureStatusTypeUtils {
	
	/** Error Logging */
	private static Log log = LogFactory.getLog(SignatureStatusTypeUtils.class);
	
	private static String XML_TAG_WORKFLOW = "workflow";
	private static String XML_TAG_STATUS_LIST = "statusTypeList";
	private static String XML_TAG_STATUS = "statusType";
	private static String XML_TAG_DISPLAY_NAME = "displayName";

	private static String XML_ATTR_WORKFLOW_NAME = "workflowDefName";
	private static String XML_ATTR_STATUS_TYPE_NAME = "statusTypeDefName";
	private static String XML_ATTR_DISPLAY_NAME_JA = "displayNameJA";
	private static String XML_ATTR_DISPLAY_NAME_EN = "displayNameEN";

	private static StatusTypeService statusTypeService = null;
	
	/**
	 *  電子署名用XMLからステータスタイプの表示名を返却
	 *  XMLに設定がない場合は、ステータスタイプ名を返却する
	 * @param sess セッション
	 * @param workflow ワークフロー
	 * @return ステータスタイプ毎の表示名マップ HashMap<String,String[]>
	 *        <key:ステータスタイプ定義名称 , value:表示名リスト>表示名リストの配列1つ目が日本語名、2つ目が英語名
	 * @throws Exception
	 */
	public static HashMap<String,String[]> getDisplayStatusName(EIMSession sess, EIMWorkFlow workflow) throws Exception {
		
		// ステータスタイプサービスを取得
		statusTypeService = (StatusTypeService)ApplicationContextLoader.getApplicationContext().getBean("statusTypeService2");
		
		HashMap<String,String[]> resultStatusTyoeNameMap = new HashMap<String,String[]>();

		// ステータスタイプ名マッピング設定に従わない場合
		if(!Boolean.valueOf(EIMConfig.get("SIGNATURE_STATUS_TYPE_CONF_DEFAURT_USE"))) {
			// 全てのステータスタイプ名をそのまま返却する
			resultStatusTyoeNameMap = getAllStatusTypeMap(sess,workflow);
			return resultStatusTyoeNameMap;
		}

		// XML読み込み
		HashMap<String , HashMap<String,String[]>> readXMLMap = readXML();

		// XMLに合致するワークフローがない場合
		if(!readXMLMap.containsKey(workflow.getDefName())){
			// 電子署名用設定 {0} にワークフロー定義名称 {1} が存在しないため、システム管理で設定したステータスタイプを表示します。
			log.warn(EIMResource.getMessage(sess,"EIM.WARN.LOGIC.SIGNATURE.XML.NO.WORKFLOW", new Object[]{SignatureStatusTypeConfig.getInstance().getFilePath(), workflow.getDefName()}));
			// 全てのステータスタイプ名をそのまま返却する
			resultStatusTyoeNameMap = getAllStatusTypeMap(sess,workflow);
			return resultStatusTyoeNameMap;
		}

		// XMLに合致するワークフローがある場合
		HashMap<String,String[]> xmlStatuTypeMap = readXMLMap.get(workflow.getDefName());
		List statusTypeList = workflow.getStatusTypeList();
		for(int i = 0; i < statusTypeList.size(); i++){
			EIMStatusType statusType = (EIMStatusType)statusTypeList.get(i);
			// 公開済、公開処理中ステータスタイプの場合はチェック処理を行わない
			if(statusType.getKind() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC || statusType.getKind() == AppConstant.STATUS_TYPE_KIND_ID_PUBLIC ){
				continue;
			}
			// XMLに合致するステータスタイプ定義名称がない場合
			if(!xmlStatuTypeMap.containsKey(statusType.getDefName())){
				// 電子署名用設定 {0} にステータスタイプ定義名称 {1} が存在しないため、システム管理で設定したステータスタイプを表示します。
				log.warn(EIMResource.getMessage(sess,"EIM.WARN.LOGIC.SIGNATURE.XML.NO.STATUSTYPE", new Object[]{SignatureStatusTypeConfig.getInstance().getFilePath(), statusType.getDefName()}));
				resultStatusTyoeNameMap.put(statusType.getDefName(), getStatusTypeNames(sess,statusType));

			}else{
				// XMLから取得した表示名称を設定
				resultStatusTyoeNameMap.put(statusType.getDefName(), xmlStatuTypeMap.get(statusType.getDefName()));
			}
		}
		return resultStatusTyoeNameMap;
	}
	
	
	/**
	 *  電子署名用XML読み込み
	 * @return HashMap<String , HashMap<String,String[]>> XML読み込み結果
	 * HashMap<key:WF定義名称 , value: HashMap<key:ステータスタイプ定義名称 , value:表示名リスト>>
	 *
	 * @throws Exception
	 */
	private static HashMap<String , HashMap<String,String[]>> readXML() throws Exception {
		// 返却用マップ
		HashMap<String , HashMap<String,String[]>> readXMLMap = new HashMap<String , HashMap<String,String[]>>();

		// ワークフロー定義名称重複チェック用セット
		Set<String> workflowDefNameDupCheckSet = new HashSet<String>();
		// ステータスタイプ定義名称重複チェック用セット
		Set<String> statusTypeDefNameDupCheckSet = new HashSet<String>();

		// XML読み込み
		Document doc = null;
		try {
			doc = SignatureStatusTypeConfig.getInstance().getDocument();
		} catch (SAXParseException se) {
			// 電子署名用設定 {0}が不正です。
			se.printStackTrace();
			throw new EIMException("EIM.ERROR.LOGIC.SIGNATURE.XML", new Object[]{SignatureStatusTypeConfig.getInstance().getFilePath()});
		} catch (Exception e) {
			// システムエラーが発生しました。
			e.printStackTrace();
			throw new EIMException("EIM.ERROR.SYSTEMERROR");
		}


		Element rootElement = doc.getDocumentElement();
		NodeList workflowListNodeList = rootElement.getElementsByTagName(XML_TAG_WORKFLOW);

		// ワークフローリスト取得
		for(int i = 0; i < workflowListNodeList.getLength(); i++){
			Node workflowNode = workflowListNodeList.item(i);

			Element wfElement = (Element)workflowNode;
			String workflowDefName = wfElement.getAttribute(XML_ATTR_WORKFLOW_NAME);

			// ワークフロー定義名称 重複チェック
			if(workflowDefNameDupCheckSet.contains(workflowDefName)){
				// 電子署名用設定 {0} でワークフロー定義名称 {1} が重複して設定されています。
				throw new EIMException("EIM.ERROR.LOGIC.SIGNATURE.XML.DUPLICATE.WORKFLOW", new Object[]{SignatureStatusTypeConfig.getInstance().getFilePath(), workflowDefName});
			}
			workflowDefNameDupCheckSet.add(workflowDefName);
			// 返却マップに格納
			readXMLMap.put(workflowDefName, new HashMap<String,String[]>());

			// ステータスタイプリスト取得
			Node statusTypeListNode = wfElement.getElementsByTagName(XML_TAG_STATUS_LIST).item(0);
			NodeList statusTypeListChildren = ((Element)statusTypeListNode).getElementsByTagName(XML_TAG_STATUS);


			for(int j = 0; j < statusTypeListChildren.getLength(); j++){
				Node statusTypeNode = statusTypeListChildren.item(j);
				String statusTypeDefName = ((Element)statusTypeNode).getAttribute(XML_ATTR_STATUS_TYPE_NAME);
				// ステータスタイプ定義名称 重複チェック
				if(statusTypeDefNameDupCheckSet.contains(statusTypeDefName)){
					// 電子署名用設定 {0} のワークフロー {1} 定義でステータスタイプ定義名称 {2} が重複して設定されています。
					throw new EIMException("EIM.ERROR.LOGIC.SIGNATURE.XML.DUPLICATE.STATUSTYPE", new Object[]{SignatureStatusTypeConfig.getInstance().getFilePath(),workflowDefName, statusTypeDefName});
				}
				statusTypeDefNameDupCheckSet.add(statusTypeDefName);
				// 表示名取得
				Node displayNameNode = ((Element)statusTypeNode).getElementsByTagName(XML_TAG_DISPLAY_NAME).item(0);
				if(displayNameNode == null){
					// 電子署名用設定 {0} のワークフロー {1} のステータスタイプ {2} に表示名称が設定されていません。
					throw new EIMException("EIM.ERROR.LOGIC.SIGNATURE.XML.NO.SET.DISPLAYNAME", new Object[]{SignatureStatusTypeConfig.getInstance().getFilePath(),workflowDefName, statusTypeDefName});

				}
				String displayNameJA = ((Element)displayNameNode).getAttribute(XML_ATTR_DISPLAY_NAME_JA);
				String displayNameEN = ((Element)displayNameNode).getAttribute(XML_ATTR_DISPLAY_NAME_EN);

				String[] displaeNames = new String[2];
				displaeNames[0] = displayNameJA;
				displaeNames[1] = displayNameEN;
				// 返却用マップに格納
				HashMap<String,String[]> statusTypeMap = readXMLMap.get(workflowDefName);
				statusTypeMap.put(statusTypeDefName, displaeNames);
			}
			statusTypeDefNameDupCheckSet.clear();
		}
		return readXMLMap;
	}
	
	/**
	 * 引数のワークフローがDB上で保持している全てのステータスタイプ名(日本語、英語)を返却する
	 * @param sess
	 * @param workflow
	 * @return ステータスタイプ毎の名称マップ HashMap<Integer,String[]>
	 *        <key:ステータスタイプ定義名称 , value:名称リスト> 名称リストの配列1つ目が日本語名、2つ目が英語名
	 * @throws Exception
	 */

	private static HashMap<String,String[]> getAllStatusTypeMap(EIMSession sess, EIMWorkFlow workflow) throws Exception {
		HashMap<String,String[]> resultStatusTyoeNameMap = new HashMap<String,String[]>();
		List statusTypeList = workflow.getStatusTypeList();
		for(int i = 0; i < statusTypeList.size(); i++){
			EIMStatusType statusType = (EIMStatusType)statusTypeList.get(i);
			// 公開済、公開処理中ステータスタイプの場合はチェック処理を行わない
			if(statusType.getKind() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC || statusType.getKind() == AppConstant.STATUS_TYPE_KIND_ID_PUBLIC ){
				continue;
			}
			// ステータスタイプメインを取得(単一取得メソッドでないと全言語の名称を返さない)
			StatusTypeDomain statusTypeDomain = statusTypeService.getById(statusType.getId());
			String[] statusTypeNames = new String[2];
			// ステータスタイプの日本語、英語名称を取得
			for(OtherNameDomain otherName:statusTypeDomain.getNameList()){
				if(otherName.getLangId().equals("JA")){
					statusTypeNames[0] = otherName.getName();
				}
				if(otherName.getLangId().equals("EN")){
					statusTypeNames[1] = otherName.getName();
				}
			}
			resultStatusTyoeNameMap.put(statusTypeDomain.getDefinitionName(), statusTypeNames);
		}
		return resultStatusTyoeNameMap;
	}
	
	/**
	 * 引数のステータスタイプ名(日本語、英語)を返却する
	 * @param sess
	 * @param workflow
	 * @param statusType
	 * @return ステータスタイプ名称リスト String[]
	 *        名称リストの配列1つ目が日本語名、2つ目が英語名
	 * @throws Exception
	 */
	private static String[] getStatusTypeNames(EIMSession sess, EIMStatusType statusType) throws Exception {
		// ステータスタイプメインを取得
		StatusTypeDomain statusTypeDomain = statusTypeService.getById(statusType.getId());
		String[] statusTypeNames = new String[2];
		// 日本語名称、英語名称を取得
		for(OtherNameDomain otherName:statusTypeDomain.getNameList()){
			if(otherName.getLangId().equals("JA")){
				statusTypeNames[0] = otherName.getName();
			}
			if(otherName.getLangId().equals("EN")){
				statusTypeNames[1] = otherName.getName();
			}
		}
		return statusTypeNames;
	}
}
