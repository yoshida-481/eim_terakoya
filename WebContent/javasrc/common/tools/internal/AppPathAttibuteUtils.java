package common.tools.internal;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMRelation;
import eim.bo.EIMRelationType;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMSearchSelectEIMRelation;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.RelationUtils;
import eim.util.SearchUtils;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.common.exception.EIMSysException;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.integration.dao.plugin.DatabasePlugIn.RecursiveTableEnum;
import jp.co.ctc_g.eim.framework2.integration.dao.plugin.DatabasePlugInLoader;

/**
 * ドキュメント管理用パス属性ユーティリティ
 *
 */
public class AppPathAttibuteUtils {
	
	private static EIMAttributeType pathAttributeType = null;
	
	private static EIMAttributeType documentLinkAttributeType = null;
	/**
	 * 対象のオブジェクトからのリレーションからたどったのパスの情報を返す。
	 * ワークスース、ごみ箱に関してはパス情報はなし。（空文字列)
	 * @return
	 * @throws Exception 
	 */
	private static EIMRelationType documentRelType = null;
	
	private static EIMRelationType linkRelType = null;
	
	//最新のVIDとIDのHashMap
	private static Map<Integer,Integer> latestObjectMap = null;
	
	/**
	 * 全てのオブジェクトのIDとVIDのマップ
	 */
	private static Map<Integer,Integer> allIdVidMap = null;
	
	
	public static List<PathDomain> getPathByRelation(EIMObject object,boolean withAttributeFlag) throws Exception{
		initLatestObjectMapping();
		List<PathDomain> resultList = new ArrayList<PathDomain>();
		//ワークスペース、ごみ箱の場合、指定したオブジェクトの情報のみ返却する
		if(isRootNode(object)){
			PathDomain ph = new PathDomain();
			ph.setRealPath("");
			ph.setLeafObject(object);
			ph.setType(PathTypeEnum.ROOT);
			List<EIMObject> list = new ArrayList<EIMObject>();
			list.add(object);
			ph.setObjectPath(list);
			resultList.add(ph);
			return resultList;
		}
		
		EIMSession sess = EIMThreadContext.getEIMSession();
		if(documentRelType == null){
			documentRelType = RelationUtils.getRelationTypeByName(sess, EIMConfig.getValue("RELATION_TYPE_NAME_DOCUMENT"));
		}
		if(linkRelType == null){
			linkRelType = RelationUtils.getRelationTypeByName(sess, EIMConfig.getValue("RELATION_TYPE_NAME_LINK"));
		}
		PathDomain docPath = new PathDomain();
		//最新のものがなかった場合、不正なデータであるので、パスをリカバリできない
		long latestObjectId = 0;
		if(!latestObjectMap.containsKey((allIdVidMap.get(object.getId())))){
			docPath.setType(PathTypeEnum.IRREGULAR_LATEST_NOTFOUND);
			latestObjectId = object.getId();
		}
		else{
			latestObjectId = latestObjectMap.get(allIdVidMap.get(object.getId()));
		}
		docPath.setLeafObject(object);
		ObjectDomain obj = new ObjectDomain();
		obj.setId(latestObjectId);
		EIMObject latestObject = obj.createEIMObject();
		List<EIMRelation> documentParentRelList = getParentRelationListByRelType(sess, latestObject, documentRelType, withAttributeFlag);
		if(documentParentRelList.size() == 0){
			List<EIMObject> resultObject = new ArrayList<EIMObject>();
			resultObject.add(object);
			docPath.setObjectPath(resultObject);
			docPath.setType(PathTypeEnum.IRREGULAR_NORELATION);
			resultList.add(docPath);
			return resultList;
		}
		//ルート以外のドキュメントリレーションは必ず１つ存在する
		List<EIMObject> objList = getParentObjectByReflexive(documentParentRelList.get(0).getParent(), withAttributeFlag);
		objList.add(objList.size(), object);
		
		docPath.setObjectPath(objList);
		resultList.add(docPath);
		if(documentParentRelList.get(0).getParent().getType().getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_MYDOCUMENT"))){
			docPath.setType(PathTypeEnum.MYDOCUMENT);
			return resultList;
		}
		else if(docPath.getType() ==  null){
			docPath.setType(PathTypeEnum.DOCUMENT);
		}
		
		if(objList.size() == 2){
			docPath.setRealPath("/"+objList.get(0).getName() + "/");
		}
		else{
			for(int i = 0 ; i < objList.size()-1 ; i++){
				if(i == 0){
					docPath.setRealPath("/"+ objList.get(i).getName() + "/");
				}
				else{
					docPath.setRealPath(docPath.getRealPath() + objList.get(i).getName() + "/");
				}
			}
		}
		List<EIMRelation> linkParentRelList = getParentRelationListByRelType(sess, object, linkRelType, withAttributeFlag);
		for(EIMRelation rel : linkParentRelList){
			List<EIMObject> linkedeObjList = getParentObjectByReflexive(rel.getParent(), withAttributeFlag);
			linkedeObjList.add(linkedeObjList.size(),object);
			PathDomain linkPath = new PathDomain();
			linkPath.setLeafObject(object);
			linkPath.setType(PathTypeEnum.LINK);
			linkPath.setObjectPath(linkedeObjList);
			resultList.add(linkPath);
			if(objList.size() == 2){
				linkPath.setRealPath("/"+linkedeObjList.get(0).getName() + "/");
			}
			else{
				for(int i = 0 ; i < linkedeObjList.size()-1 ; i++){
					if(i == 0){
						linkPath.setRealPath("/"+ linkedeObjList.get(i).getName() + "/");
					}
					else{
						linkPath.setRealPath(linkPath.getRealPath() + linkedeObjList.get(i).getName() + "/");
					}
				}
			}
		}
		return resultList;
	}
	
	/**
	 * getPathByRelationを正しく呼ぶ為の初期化処理
	 * @throws Exception
	 */
	private synchronized static void initLatestObjectMapping() throws Exception{
		if(latestObjectMap != null){
			return;
		}
		EIMSession sess = EIMThreadContext.getEIMSession();
		String sql = "select id,vid from eimobj,eimver where eimobj.id = eimver.oid and eimobj.latest = 1";
		PreparedStatement ps = sess.getDBConnection().prepareStatement(sql);
		ResultSet rs = ps.executeQuery();
		latestObjectMap = new HashMap<Integer,Integer>();
		while (rs.next()) {
			latestObjectMap.put(rs.getInt("vid"),rs.getInt("id"));
		}
		rs.close();
		ps.cancel();
		
		sql = "select id,vid from eimobj,eimver where eimobj.id = eimver.oid";
		ps = sess.getDBConnection().prepareStatement(sql);
		rs = ps.executeQuery();
		allIdVidMap = new HashMap<Integer,Integer>();
		while (rs.next()) {
			allIdVidMap.put(rs.getInt("id"),rs.getInt("vid"));
		}
		rs.close();
		ps.cancel();
		
	}
	/**
	 * オブジェkとのパス属性、リンク関連属性を削除する
	 * @throws Exception
	 */
	public static void deleteDocumentLinkAndPathAttribute() throws Exception{
		try{
			EIMSession sess = EIMThreadContext.getEIMSession();
			String sql = "delete from EIMOBJINT where EIMOBJINT.type = (select id from EIMATTR where name = '" + EIMConfig.getValue("ATTR_NAME_DOCUMENT_DOCUMENT_LINK") +"')";
			PreparedStatement ps = sess.getDBConnection().prepareStatement(sql);
			ps.execute();
			ps.close();
			sql = "delete from EIMOBJSTR where EIMOBJSTR.type = (select id from EIMATTR where name = '" + EIMConfig.getValue("ATTR_NAME_DOCUMENT_PASS") +"')";
			ps = sess.getDBConnection().prepareStatement(sql);
			ps.execute();
			ps.close();
			sql = "delete from EIMOBJINT where EIMOBJINT.type = (select id from EIMATTR where name = '" + EIMConfig.getValue("ATTR_NAME_DOCUMENT_TARGET_TO_LINK") +"')";
			ps = sess.getDBConnection().prepareStatement(sql);
			ps.execute();
			ps.close();
		}
		catch(Exception e){
			new EIMSysException(e.getMessage());
		}
	}
	private static void setPathDomainInfo(EIMObject object, PathDomain path,boolean withAttributeFlag) throws Exception{
		EIMSession sess = EIMThreadContext.getEIMSession();
		path.getObjectPath().add(0, object);
		List<EIMRelation> parentRelList = getParentRelationListByRelType(sess, object, documentRelType, withAttributeFlag);
		for(EIMRelation pRel : parentRelList){
			EIMObject parentObject = pRel.getParent();
			if(isRootNode(parentObject)){
				path.setRealPath("/" + parentObject.getName() + "/"+path.getRealPath());
				path.getObjectPath().add(0, parentObject);
			}
			else{
				path.setRealPath(parentObject.getName() + "/" + path.getRealPath());
				setPathDomainInfo(parentObject, path, withAttributeFlag);
			}
		}
	}
	/**
	 * ルートノード（ワークスペースかごみ箱）を判別する
	 * @param object 判別するオブジェクト
	 * @return ルートノードかどうか<br>
	 * ルートノードであれば、trueを返す。
	 * @throws EIMException
	 */
	private static boolean isRootNode(EIMObject object) throws EIMException{
		return object.getType().getDefName().equals(EIMConfig.getValue("OBJECT_TYPE_NAME_WORKSPACE")) || 
				object.getType().getDefName().equals(EIMConfig.getValue("OBJECT_TYPE_NAME_RECYCLE"));
	}

	private static List<EIMRelation> getParentRelationListByRelType(EIMSession sess, EIMObject childObj, EIMRelationType relType,boolean withAttributeFlag) throws Exception
	{
		if(pathAttributeType == null)
			pathAttributeType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.getValue("ATTR_NAME_DOCUMENT_PASS"));
		if(documentLinkAttributeType == null)
			documentLinkAttributeType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.getValue("ATTR_NAME_DOCUMENT_TARGET_TO_LINK"));
		EIMSearchSelectEIMRelation relationSelectTarget = new EIMSearchSelectEIMRelation();
        eim.bo.EIMSearchSelectEIMRelation.SearchConditionBuildHelper h = new eim.bo.EIMSearchSelectEIMRelation.SearchConditionBuildHelper();
        relationSelectTarget.setCondition(h.group(h.opAnd()).addCondition(
        		h.eq(
        				h.opAnd(), eim.bo.EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.CHILD, childObj.getId())).addCondition(
        		h.eq(
        				h.opAnd(), eim.bo.EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.TYPE, relType.getId())));
        relationSelectTarget.setResultAttrs(new ArrayList());
        List<EIMAttributeType> resultAttrList = new ArrayList<EIMAttributeType>();
        if(withAttributeFlag){
        	resultAttrList.add(pathAttributeType);
        	resultAttrList.add(documentLinkAttributeType);
        }
        resultAttrList.add(EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME);
        resultAttrList.add(EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE);
        EIMSearchSelectEIMObject parentSelectTarget = new EIMSearchSelectEIMObject();
        
        parentSelectTarget.setResultAttrs(resultAttrList);
        EIMSearchSelectEIMObject childSelectTarget = new EIMSearchSelectEIMObject();
        childSelectTarget.setResultAttrs(new ArrayList());
        return SearchUtils.searchRelations(sess, relationSelectTarget, parentSelectTarget, childSelectTarget, null);
	}

	private static List<EIMObject> getParentObjectByReflexive(EIMObject object,boolean withAttributeFlag) throws Exception {
		List<EIMObject> result = new ArrayList<EIMObject>();

		String[] columns1 = {"parent", "type"};
		String[] columns2 = {"parent", "type", "level lv"};
		String startCondition = "child = ? and type = ?";
		String connectCondition = "EIMREL.type = ?";

		String sql =
				"select id, name, lv " +
				"from " +
				"eimobj, " +
				"(" +
					DatabasePlugInLoader.getPlugIn().getQueryStringWithRecursive(RecursiveTableEnum.EIMREL_PARENTS, columns1, startCondition, connectCondition, false) +
					DatabasePlugInLoader.getPlugIn().getQueryStringSelectRecursive(RecursiveTableEnum.EIMREL_PARENTS, columns2, startCondition, connectCondition, null, false) +
				") recOBJ " +
				"where " +
				"eimobj.id = recOBJ.parent " +
				"ORDER BY " +
				"recOBJ.lv";

			EIMSession sess = EIMThreadContext.getEIMSession();
			
			PreparedStatement ps = sess.getDBConnection().prepareStatement(sql);
			ps.setLong(1, object.getId());
			ps.setLong(2, documentRelType.getId());
			ps.setLong(3, documentRelType.getId());
			ResultSet rset = ps.executeQuery();
			while (rset.next()) {
				EIMObject ob = new EIMObject(rset.getInt("id"),
						null,
						rset.getString("name"),
						0,
						false,
						null,
			            null,
			            null,
			            null,
			            null,
			            null,
			            false,
			            false, 
			            null
						);
				result.add(0,ob);
			}
			rset.close();
			ps.close();
			
			if(result.size() > 0 && withAttributeFlag){
				//QueryBuilderAndLoaderEIMOBJ
				AttributeLoader attrLoader = new AttributeLoader();
				attrLoader._resultAttrTypes.add(pathAttributeType);
				attrLoader._resultAttrTypes.add(documentLinkAttributeType);
				attrLoader.loadAttrToObj(sess, result);
			}
			result.add(result.size(), object);
		return result;
	}
}
