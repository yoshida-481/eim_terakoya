package common.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import app.document.object.FixedForm;
import eim.bo.EIMObjectType;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.NamingDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.SequenceDomain;
import jp.co.ctc_g.eim.framework2.business.service.NamingService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;

/**
 * 
 * (ドキュメント管理用)オブジェクトタイプ関連クラス
 *
 */
public class AppObjectTypeUtil {

	/**
	 * 指定したルートオブジェクトタイプ配下の全オブジェクトタイプのマップを取得します。<br>
	 * key：EIMOBJTYPEのID（Integer）<br>
	 * value：EIMObjectTypeのインスタンスとなります。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param rootObjTypeName トップのオブジェクトとタイプ
	 * @return 取得したオブジェクト
	 * @throws Exception
	 */
	public 	static Map getObjTypeMap(EIMSession sess, String rootObjTypeName) throws Exception
	{
		Map retMap = new HashMap();
		
		EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess, rootObjTypeName);
		
		return getObjTypeMap(sess, retMap, objType);
				
	}
	
	private static Map getObjTypeMap(EIMSession sess, Map retMap, EIMObjectType objType) throws Exception
	{
		retMap.put(new Long(objType.getId()), objType);

		//Child Object Type
		List objTypeList = ObjectUtils.getChildObjectTypeList(sess, objType);
		for(int i = 0; i < objTypeList.size(); i++)
		{
			//Object Type
			EIMObjectType childObjType = (EIMObjectType)objTypeList.get(i);

			getObjTypeMap(sess, retMap, childObjType);
		}
		
		return retMap;
	}
	
	/**
	 * ドキュメントの定型フォームを取得
	 * @param sess セッション情報
	 * @return ドキュメントとそのサブクラスの定型フォーム
	 * @throws Exception
	 */
	public static FixedForm getDocumentFixedForm(EIMSession sess) throws Exception
	{
		return getFixedForm(sess, "OBJECT_TYPE_NAME_DOCUMENT");
	}
	
	/**
	 * フォルダの定型フォームを取得
	 * @param sess セッション情報
	 * @return フォルダとそのサブクラスの定型フォーム
	 * @throws Exception
	 */
	public static FixedForm getFolderFixedForm(EIMSession sess) throws Exception
	{
		return getFixedForm(sess, "OBJECT_TYPE_NAME_FOLDER");
	}
	
	/**
	 * タグの定型フォームを取得
	 * @param sess セッション情報
	 * @return タグとそのサブクラスの定型フォーム
	 * @throws Exception
	 */
	public static FixedForm getTagFixedForm(EIMSession sess) throws Exception
	{
		return getFixedForm(sess, "OBJECT_TYPE_NAME_TAG");
	}
	
	
	/**
	 * 定型ドキュメントを階層構造を含め取得
	 * @param sess セッション情報
	 * @param rootTypeKey 
	 * @return
	 * @throws Exception
	 */
	private static FixedForm getFixedForm(EIMSession sess, String rootTypeKey) throws Exception
	{
		// ルートのFixedFormを定義
		EIMObjectType rootType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get(rootTypeKey));
		FixedForm root = new FixedForm();
		root.setRootType(root);
		root.setParent(null);
		root.setFormType(rootType);
		
		getChildFixedForm(sess, root);
		
		return root;
	}
	
	/**
	 * 
	 * @param sess セッション情報
	 * @param parent 親のFixedForm
	 * @return 戻り値
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	private static void getChildFixedForm(EIMSession sess, FixedForm parent) throws Exception
	{
		// 子のオブジェクトタイプを取得
		List<EIMObjectType> objTypeList = ObjectUtils.getChildObjectTypeList(sess, parent.getFormType());
		List<FixedForm> childFormList = new ArrayList<FixedForm>();
		
		for (EIMObjectType type : objTypeList) {
			
			// FixedForm作成
			FixedForm child = new FixedForm();
			child.setParent(parent);
			child.setRootType(parent.getRootType());
			child.setFormType(type);
			
			// 子のFixedFormを作成
			getChildFixedForm(sess, child);
			
			// リストに追加
			childFormList.add(child);
		}
		
		parent.setChildType(childFormList);
	}
	
	public static FixedForm chkDspFlag(FixedForm form, List<Long> idList) throws Exception
	{
		if(idList.indexOf(new Long(form.getFormType().getId())) != -1)
		{
			form.setDspFlag(true);
			dspChildFixedForm(form);
		}
		else
		{
			chkChildFlag(form, idList, false);
		}
		return form;
	}
	
	/**
	 * 指定されたFixedForm以下のFixedFormのdspFlagとselectedFlagの値を
	 * idのリストの値に応じて設定します。（idが含まれていたら"true"）
	 * @param form FixedForm
	 * @param idList 使用可能タイプのIDを格納したリスト
	 * @return 
	 * @throws Exception
	 */
	public static FixedForm chkDspAndSelectedFlag(FixedForm form, List<Long> idList) throws Exception
	{
		if(idList.indexOf(new Long(form.getFormType().getId())) != -1)
		{
			form.setDspFlag(true);
			form.setSelectedFlag(true);
			dspChildFixedForm(form);
		}
		else
		{
			chkChildFlag(form, idList, true);
		}
		
		if(idList.size() != 0){
			//削除されたタイプが含まれる
			form.setDeletedTypeCount(idList.size());
		}
		
		return form;
	}
	
	/**
	 * 指定されたFixedFormの子のFlagを設定します。
	 * @param parent 親のFixedForm
	 * @param idList 使用可能タイプのIDを格納したリスト
	 * @param chkSelectedFlag selectedFlagもチェックするか否か
	 * @throws Exception
	 */
	private static void chkChildFlag(FixedForm parent, List<Long> idList, boolean chkSelectedFlag) throws Exception
	{
		for(FixedForm child : parent.getChildType())
		{
			int index = idList.indexOf(new Long(child.getFormType().getId()));
			if(index != -1)
			{
				// dspFlagの設定
				child.setDspFlag(true);
				idList.remove(index);
				dspChildFixedForm(child);
				dspParentFixedForm(child);
				// selectedFlagの設定
				if(chkSelectedFlag){
					child.setSelectedFlag(true);
					// さらに配下に選択済みのものがあればチェック
					chkChildSelectedFlag(child, idList);
				}
			}
			else
			{
				chkChildFlag(child, idList, chkSelectedFlag);
			}
		}
	}
	
	/**
	 * 指定されたFixedForm以下のFixedFormのdspFlagの値を全てtrueにする
	 * 
	 * @param parent
	 * @throws Exception
	 */
	public static void dspChildFixedForm(FixedForm parent) throws Exception
	{
		for(FixedForm child : parent.getChildType())
		{
			child.setDspFlag(true);
			dspChildFixedForm(child);
		}
	}
	
	/**
	 * 指定されたFixedFormより親のFixedFormのdspFlagの値を全てtrueにする
	 * 
	 * @param child
	 * @throws Exception
	 */
	public static void dspParentFixedForm(FixedForm child) throws Exception
	{
		FixedForm parent = child.getParent();
		if(parent != null){
			parent.setDspFlag(true);
			dspParentFixedForm(parent);
		}
	}
	
	/**
	 * オブジェクトタイプに設定した連続データIDを取得する
	 * @param id オブジェクトタイプID
	 * @return 連続データID
	 * @throws Exception
	 */
	public static String getNextValue(long objectTypeId) throws Exception
	{
		String nextValue = null;
		
		// サービス
		ObjectTypeService objectTypeService = (ObjectTypeService)ApplicationContextLoader.getContext().getBean("objectTypeService2");
		NamingService namingService = (NamingService)ApplicationContextLoader.getContext().getBean("namingService2");
		
		// オブジェクトタイプ取得
		ObjectTypeDomain objectType = objectTypeService.getById(objectTypeId);
		
		// オブジェクトタイプオブジェクト取得
		ObjectDomain objectTypeObject = objectType.getObjectTypeObject();
		
		// 連続データID属性
		AttributeDomain attribute = objectTypeObject.getAttribute(ConfigUtils.getByKey("ATTRIBUTE_TYPE_NAME_SEQUENCE_ID"));
		
		if (attribute != null) {
			
			// 発番
			NamingDomain naming = new NamingDomain();
			naming.setSequence(new SequenceDomain(attribute.getLong()));
			try {
				nextValue = namingService.generate(naming);
			} catch (EIMException e) {
				e.printStackTrace();
			}
			
		}
		
		return nextValue;
	}
	
	/**
	 * 指定されたFixedFormの配下に選択済みに指定された(idListにIDが格納されている)子があれば
	 * selectedFlagの値をtrueにする
	 * 
	 * @param form
	 * @param idList
	 * @throws Exception
	 */
	private static void chkChildSelectedFlag(FixedForm form, List<Long> idList) throws Exception
	{
		if(idList.size() != 0){
			for(FixedForm child : form.getChildType())
			{
				int index = idList.indexOf(new Long(child.getFormType().getId()));
				if(index != -1)
				{
					// selectedFlagの設定
					child.setSelectedFlag(true);
					idList.remove(index);
					// 空になったらおしまい
					if(idList.size() == 0){
						break;
					}else{
						chkChildSelectedFlag(child, idList);
					}
				}
				else
				{
					chkChildSelectedFlag(child, idList);
				}
			}
		}
	}

}