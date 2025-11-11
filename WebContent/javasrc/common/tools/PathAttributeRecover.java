package common.tools;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import common.tools.internal.AppObjectTypeUtils;
import common.tools.internal.AppPathAttibuteUtils;
import common.tools.internal.PathDomain;
import common.tools.internal.PathTypeEnum;
import common.util.AppMessageUtils;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchSelectEIMObject;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.ObjectAttributeUtils;
import eim.util.SearchUtils;
import eim.util.TypeConvertUtils;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * パス属性をリレーションから修復するクラス
 *
 */
public class PathAttributeRecover {

	/**
	 * パス属性をリレーションから修復する<br>
	 * @param args
	 */
	private static EIMAttributeType pathAttributeType = null;
	
	private static EIMAttributeType documentLinkAttributeType = null;
	
	private static EIMAttributeType documentLinkFlagAttributeType = null;
	
	private static Log log = null;
	private static boolean isTestMode = false;
	/**
	 * ドキュメントの属性(パス、リンク先)を修復する。<br>
	 * args[0]=どのオブジェクトタイプのオブジェクトを修復するか<br>
	 * all=属性の消去と、すべてのドキュメント管理管理下のオブジェクトの属性を修復する
	 * delete=パス属性、リンク属性を消去する<br>
	 * document=ドキュメントの属性を修復する<br>
	 * folder=フォルダの属性を修復する<br>
	 * tag=タグの属性を修復する。<br>
	 * args[1]=コミットせずにループさせる場合は、trueにする(非必須)
	 * @param args
	 */
	public static void main(String[] args) {
		if(args.length == 0){
			System.out.println("arge error");
			return;
		}
		if(args.length > 1 && args[1].equalsIgnoreCase("TEST")){
			isTestMode = true;
		}
		//isTestMode = true;
		EIMSession sess = null;
		log = LogFactory.getLog(PathAttributeRecover.class.toString() + ":" + args[0]);
		log.info(" start pathAttributeRecover "+ args[0]);
		try
		{
			sess = new EIMSession();
			sess.setConsole();
			boolean isDocumentExecute = false;
			boolean isFolderExecute = false;
			boolean isTagExecute = false;
			boolean isDeleteExecute = false;
			EIMThreadContext.putEIMSession(sess);
			if("all".equalsIgnoreCase(args[0])){
				isDocumentExecute = true;
				isFolderExecute = true;
				isTagExecute = true;
				isDeleteExecute = true;
			}
			else if("document".equalsIgnoreCase(args[0])){
				isDocumentExecute = true;
			}
			else if("folder".equalsIgnoreCase(args[0])){
				isFolderExecute = true;
			}
			else if("tag".equalsIgnoreCase(args[0])){
				isTagExecute = true;
			}
			else if("delete".equalsIgnoreCase(args[0])){
				isDeleteExecute = true;
			}
			if(isDeleteExecute){
				AppPathAttibuteUtils.deleteDocumentLinkAndPathAttribute();
				sess.commit();
			}
			if("delete".equalsIgnoreCase(args[0])){
				return;
			}
			pathAttributeType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.getValue("ATTR_NAME_DOCUMENT_PASS"));
			documentLinkAttributeType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.getValue("ATTR_NAME_DOCUMENT_TARGET_TO_LINK"));
			documentLinkFlagAttributeType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.getValue("ATTR_NAME_DOCUMENT_DOCUMENT_LINK"));
			if(isFolderExecute){
				log.info(" start folder");
				List<Integer> workSpaceIds = AppObjectTypeUtils.getListByInheritanceTypeName(EIMConfig.getValue("OBJECT_TYPE_NAME_WORKSPACE"));
				pathAttributeRecoving(workSpaceIds);
			}
			if(isDocumentExecute){
				log.info(" start document");
				List<Integer> documentIds = AppObjectTypeUtils.getListByInheritanceTypeName(EIMConfig.getValue("OBJECT_TYPE_NAME_DOCUMENT"));
				pathAttributeRecoving(documentIds);
			}
			if(isTagExecute){
				log.info(" start tag");
				List<Integer> tagIds = AppObjectTypeUtils.getListByInheritanceTypeName(EIMConfig.getValue("OBJECT_TYPE_NAME_TAG"));
				pathAttributeRecoving(tagIds);
			}	
		}
		catch(Exception eime){
			log.warn(AppMessageUtils.makeLogMessage(eime.getMessage()), eime);
			try{
				EIMThreadContext.removeEIMSession();
				if(sess != null){
					sess.rollback();
				}
			}
			catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
			}
		}
		finally{
			try{
				if(sess != null)
					sess.close();
			}
			catch(Exception e){
			}
			log.info(" end pathAttributeRecover " + args[0]);
		}

	}
	private static boolean isRootTypeObject(EIMObject object) throws EIMException{
		return object.getType().getDefName().equals(EIMConfig.getValue("OBJECT_TYPE_NAME_WORKSPACE")) || 
			object.getType().getDefName().equals(EIMConfig.getValue("OBJECT_TYPE_NAME_RECYCLE"));
	}
	
	private static void pathAttributeRecoving(List<Integer> objIds) throws Exception{
		EIMSession sess = EIMThreadContext.getEIMSession();
		for(int objId : objIds){
			EIMSearchSelectEIMObject.SearchConditionBuildHelper h = new EIMSearchSelectEIMObject.SearchConditionBuildHelper();
			EIMSearchSelectEIMObject objSelectTarget = new EIMSearchSelectEIMObject();
			EIMSearchConditionGroup conds = h.group(h.opAnd());
			conds.addCondition(h.eq(h.opAnd(),EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID,objId));
			objSelectTarget.setCondition(conds);
			List<EIMAttributeType> resultAttrList = new ArrayList<EIMAttributeType>();
	        resultAttrList.add(EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME);
	        resultAttrList.add(EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE);
	        resultAttrList.add(EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.REV);
	        objSelectTarget.setResultAttrs(resultAttrList);
	        EIMObject object =  (EIMObject)SearchUtils.searchObjects(sess, objSelectTarget, null).get(0);
			
			//ルートの場合は何も処理しない
			if(isRootTypeObject(object)){
				continue;
			}
			List<PathDomain> pathInfoList = AppPathAttibuteUtils.getPathByRelation(object, true);
			if(pathInfoList.size() == 1){
				PathDomain pth = pathInfoList.get(0);
				if(pth.getType() == PathTypeEnum.IRREGULAR_NORELATION){
					log.warn("IRREGULAR_NORELATION:ID["+pathInfoList.get(0).getLeafObject().getId()+"]");
					continue;
				}
				else if(pth.getType() == PathTypeEnum.MYDOCUMENT){
					continue;
				}
			}
			String[] pathArray = new String[pathInfoList.size()];
			long[] linkIds = null;
			if(pathInfoList.size() -1 >0){
				linkIds = new long[pathInfoList.size() -1];
			}
			//実体のパスをパス属性の0番目に入れる
			for(int i = 0 ; i < pathInfoList.size() ; i++){
				if(pathInfoList.get(i).getType() == PathTypeEnum.DOCUMENT || pathInfoList.get(i).getType() == PathTypeEnum.IRREGULAR_LATEST_NOTFOUND){
					pathArray[0] = pathInfoList.get(i).getRealPath();
					pathInfoList.remove(i);
					break;
				}
			}
			//残りはリンクの情報なので、１件以上あればそれはリンク情報
			boolean hasLink = false;
			if(pathInfoList.size() > 0){
				hasLink = true;
			}
			for(int i = 0 ; i < pathInfoList.size() ; i++){
				pathArray[i+1] = pathInfoList.get(i).getRealPath();
				List<EIMObject> objectPathList = pathInfoList.get(i).getObjectPath();
				linkIds[i] = objectPathList.get(objectPathList.size() - 2).getId();
			}

			if(hasLink){
				ObjectAttributeUtils.setAttribute(sess, object, documentLinkFlagAttributeType, 1);
			}
			if(pathArray != null &&  pathArray.length > 0){
				ObjectAttributeUtils.setAttribute(sess, object, pathAttributeType, pathArray);
			}
			if(linkIds != null && linkIds.length > 0){
				ObjectAttributeUtils.setAttribute(sess, object, documentLinkAttributeType, TypeConvertUtils.convertToBuildTypeArray(linkIds));
			}
			if(!isTestMode){
				sess.commit();
			}

		}
	}
}
