package jp.co.ctc_g.eim.app.document.common.aop.advice;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jp.co.ctc_g.eim.app.document.business.domain.DocumentDomain;
import jp.co.ctc_g.eim.app.document.business.domain.FolderDomain;
import jp.co.ctc_g.eim.app.document.common.util.AppDocumentUtil;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.common.aop.advice.MultipleOperationHistoryAdvice;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;

public class MultipleDocumentOperationHistoryAdvice extends MultipleOperationHistoryAdvice {
	
	private ObjectService objectService;
	
	/**
	 * 組み込んだ対象のメソッドの特定の引数の値を元に操作履歴の出力を行います。<br>
	 * <br>
	 * 操作対象情報Aを元にパスを取得し操作履歴の出力を行います。<br>
	 *
	 * それぞれの引数に対し、ParameterConverterPlugInから適切なドメインクラスを取得し操作対象情報の出力を行います。<br>
	 * ParameterConverterPlugInの設定がない場合はメソッドの引数から取得した情報で操作対象情報の出力を行います。<br>
	 * operationHistoryServiceにて指定オブジェクトに変換ができない場合はException（オブジェクトの型が不明です。）をスローします。<br>
	 * <br>
	 *
	 * @param targetA 操作対象情報A
	 * <br>
	 * @since Ver6.2
	 * <br>
	 */
	public void createWithDetailWriteInPathFromFolderDomainID(List<?> targetA) throws Exception {
		List<FolderDomain> folderDomainList = (List<FolderDomain>)targetA;
		List<String> pathList = new ArrayList<String>();
		Iterator<FolderDomain> ite = folderDomainList.iterator();
		FolderDomain folderDomain = null;
		List<ObjectDomain> folderList = new ArrayList<ObjectDomain>();
		while(ite.hasNext()){
			folderDomain = ite.next();
			String path = "";
			if( folderDomain != null && folderDomain.getId() > 0){
				ObjectDomain object = objectService.getById(folderDomain.getId());
				AttributeDomain pathAtributeDomain = object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_PASS"));
				if(object != null && pathAtributeDomain != null){
					path = pathAtributeDomain.getStringList().get(0);
				}
			}
			pathList.add(path);
			folderList.add((ObjectDomain)folderDomain);
		}
		
		super.createWithDetail(targetA, pathList);
	}
	
	/**
	 * 組み込んだ対象のメソッドの特定の引数の値を元に操作履歴の出力を行います。<br>
	 * <br>
	 * 操作対象情報Aを元にパスを取得し操作履歴の出力を行います。<br>
	 *
	 * それぞれの引数に対し、ParameterConverterPlugInから適切なドメインクラスを取得し操作対象情報の出力を行います。<br>
	 * ParameterConverterPlugInの設定がない場合はメソッドの引数から取得した情報で操作対象情報の出力を行います。<br>
	 * operationHistoryServiceにて指定オブジェクトに変換ができない場合はException（オブジェクトの型が不明です。）をスローします。<br>
	 * <br>
	 *
	 * @param targetA 操作対象情報A
	 * <br>
	 * @since Ver6.2
	 * <br>
	 */
	public void createWithDetailWriteInPathFromDocumentDomainID(List<?> targetA) throws Exception {
		List<DocumentDomain> documentDomainList = (List<DocumentDomain>)targetA;
		List<String> pathList = new ArrayList<String>();
		Iterator<DocumentDomain> ite = documentDomainList.iterator();
		DocumentDomain documentDomain = null;
		while(ite.hasNext()){
			documentDomain = ite.next();
			String path = "";
			if( documentDomain != null && documentDomain.getId() > 0){
				ObjectDomain object = objectService.getById(documentDomain.getId());
				AttributeDomain pathAtributeDomain = object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_PASS"));
				if(object != null && pathAtributeDomain != null){
					path = pathAtributeDomain.getStringList().get(0);
				}
			}
			pathList.add(path);
		}
		
		super.createWithDetail(targetA, pathList);
	}
	
	/**
	 * 組み込んだ対象のメソッドの特定の引数の値を元に操作履歴の出力を行います。<br>
	 * <br>
	 * 操作対象情報Aを元にDB接続してObjectを再取得し操作履歴の出力を行います。<br>
	 *
	 * それぞれの引数に対し、ParameterConverterPlugInから適切なドメインクラスを取得し操作対象情報の出力を行います。<br>
	 * ParameterConverterPlugInの設定がない場合はメソッドの引数から取得した情報で操作対象情報の出力を行います。<br>
	 * operationHistoryServiceにて指定オブジェクトに変換ができない場合はException（オブジェクトの型が不明です。）をスローします。<br>
	 * <br>
	 *
	 * @param targetA 操作対象情報A
	 * <br>
	 * @since Ver6.2
	 * <br>
	 */
	public void createWithDetailWriteInPathAndRevision(List<?> targetA) throws Exception {
		List<DocumentDomain> documentDomainList = (List<DocumentDomain>)targetA;
		List<String> detailList = new ArrayList<String>();
		Iterator<DocumentDomain> ite = documentDomainList.iterator();
		ObjectDomain objectDomain = null;
		int revision = -1;
		String detail = null;
		while(ite.hasNext()){
			DocumentDomain documentDomain = ite.next();
			revision = -1;
			detail = "";
			if( documentDomain != null && documentDomain.getId() > 0){
				objectDomain = objectService.getById(documentDomain.getId());
				AttributeDomain pathAtributeDomain = objectDomain.getAttribute(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_PASS"));
				if(pathAtributeDomain != null && pathAtributeDomain.getStringList() != null && pathAtributeDomain.getStringList().size() > 0){
					detail = pathAtributeDomain.getStringList().get(0);
					revision = objectDomain.getRevision();
					if(revision > -1){
						detail += "  ,(Revision:"+revision+")";
					}
				}
			}
			
			detailList.add(detail);
		}
		
		
		super.createWithDetail(targetA, detailList);
	}
	
	/**
	 * 組み込んだ対象のメソッドの特定の引数の値を元に操作履歴の出力を行います。<br>
	 * <br>
	 * 操作対象情報Aを元にパスを取得し操作履歴の出力を行います。<br>
	 *
	 * それぞれの引数に対し、ParameterConverterPlugInから適切なドメインクラスを取得し操作対象情報の出力を行います。<br>
	 * ParameterConverterPlugInの設定がない場合はメソッドの引数から取得した情報で操作対象情報の出力を行います。<br>
	 * operationHistoryServiceにて指定オブジェクトに変換ができない場合はException（オブジェクトの型が不明です。）をスローします。<br>
	 * <br>
	 *
	 * @param targetA 操作対象情報A
	 * <br>
	 * @since Ver6.2
	 * <br>
	 */
	public void createWithDetailWriteInPathFromDocumentDomainIdToWsRecBox(List<?> targetA) throws Exception {
		
		// ワークスペースごみ箱への移動時のみ処理する
		for (int i = 0; i < targetA.size(); i++) {
			boolean isParentWsRecycleBox = AppDocumentUtil.isObjectInWsRecycle((ObjectDomain) targetA.get(i));
			if (isParentWsRecycleBox) {
				return;
			}	
		}

		List<DocumentDomain> documentDomainList = (List<DocumentDomain>)targetA;
		List<String> pathList = new ArrayList<String>();
		Iterator<DocumentDomain> ite = documentDomainList.iterator();
		DocumentDomain documentDomain = null;
		while(ite.hasNext()){
			documentDomain = ite.next();
			String path = "";
			if( documentDomain != null && documentDomain.getId() > 0){
				ObjectDomain object = objectService.getById(documentDomain.getId());
				AttributeDomain pathAtributeDomain = object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_PASS"));
				if(object != null && pathAtributeDomain != null){
					path = pathAtributeDomain.getStringList().get(0);
				}
			}
			pathList.add(path);
		}
		
		super.createWithDetail(targetA, pathList);
	}
	
	/**
	 * 組み込んだ対象のメソッドの特定の引数の値を元に操作履歴の出力を行います。<br>
	 * <br>
	 * 操作対象情報Aを元にパスを取得し操作履歴の出力を行います。<br>
	 *
	 * それぞれの引数に対し、ParameterConverterPlugInから適切なドメインクラスを取得し操作対象情報の出力を行います。<br>
	 * ParameterConverterPlugInの設定がない場合はメソッドの引数から取得した情報で操作対象情報の出力を行います。<br>
	 * operationHistoryServiceにて指定オブジェクトに変換ができない場合はException（オブジェクトの型が不明です。）をスローします。<br>
	 * <br>
	 *
	 * @param targetA 操作対象情報A
	 * <br>
	 * @since Ver6.2
	 * <br>
	 */
	public void createWithDetailWriteInPathFromDocumentDomainIdToSysRecBox(List<?> targetA) throws Exception {
		
		// システムごみ箱への移動時のみ処理する
		for (int i = 0; i < targetA.size(); i++) {
			boolean isParentWsRecycleBox = AppDocumentUtil.isObjectInWsRecycle((ObjectDomain) targetA.get(i));
			if (!isParentWsRecycleBox) {
				return;
			}
		}

		List<DocumentDomain> documentDomainList = (List<DocumentDomain>)targetA;
		List<String> pathList = new ArrayList<String>();
		Iterator<DocumentDomain> ite = documentDomainList.iterator();
		DocumentDomain documentDomain = null;
		while(ite.hasNext()){
			documentDomain = ite.next();
			String path = "";
			if( documentDomain != null && documentDomain.getId() > 0){
				ObjectDomain object = objectService.getById(documentDomain.getId());
				AttributeDomain pathAtributeDomain = object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_PASS"));
				if(object != null && pathAtributeDomain != null){
					path = pathAtributeDomain.getStringList().get(0);
				}
			}
			pathList.add(path);
		}
		
		super.createWithDetail(targetA, pathList);
	}
	
	/**
	 * 組み込んだ対象のメソッドの特定の引数の値を元に操作履歴の出力を行います。<br>
	 * <br>
	 * 操作対象情報Aを元にパスを取得し操作履歴の出力を行います。<br>
	 *
	 * それぞれの引数に対し、ParameterConverterPlugInから適切なドメインクラスを取得し操作対象情報の出力を行います。<br>
	 * ParameterConverterPlugInの設定がない場合はメソッドの引数から取得した情報で操作対象情報の出力を行います。<br>
	 * operationHistoryServiceにて指定オブジェクトに変換ができない場合はException（オブジェクトの型が不明です。）をスローします。<br>
	 * <br>
	 *
	 * @param targetA 操作対象情報A
	 * <br>
	 * @since Ver6.2
	 * <br>
	 */
	public void createWithDetailWriteInPathFromFolderDomainIdToWsRecBox(List<?> targetA) throws Exception {
		
		// ワークスペースごみ箱への移動時のみ処理する
		for (int i = 0; i < targetA.size(); i++) {
			boolean isParentWsRecycleBox = AppDocumentUtil.isObjectInWsRecycle((ObjectDomain) targetA.get(i));
			if (isParentWsRecycleBox) {
				return;
			}	
		}
		
		List<FolderDomain> folderDomainList = (List<FolderDomain>)targetA;
		List<String> pathList = new ArrayList<String>();
		Iterator<FolderDomain> ite = folderDomainList.iterator();
		FolderDomain folderDomain = null;
		List<ObjectDomain> folderList = new ArrayList<ObjectDomain>();
		while(ite.hasNext()){
			folderDomain = ite.next();
			String path = "";
			if( folderDomain != null && folderDomain.getId() > 0){
				ObjectDomain object = objectService.getById(folderDomain.getId());
				AttributeDomain pathAtributeDomain = object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_PASS"));
				if(object != null && pathAtributeDomain != null){
					path = pathAtributeDomain.getStringList().get(0);
				}
			}
			pathList.add(path);
			folderList.add((ObjectDomain)folderDomain);
		}
		
		super.createWithDetail(targetA, pathList);
	}
	
	/**
	 * 組み込んだ対象のメソッドの特定の引数の値を元に操作履歴の出力を行います。<br>
	 * <br>
	 * 操作対象情報Aを元にパスを取得し操作履歴の出力を行います。<br>
	 *
	 * それぞれの引数に対し、ParameterConverterPlugInから適切なドメインクラスを取得し操作対象情報の出力を行います。<br>
	 * ParameterConverterPlugInの設定がない場合はメソッドの引数から取得した情報で操作対象情報の出力を行います。<br>
	 * operationHistoryServiceにて指定オブジェクトに変換ができない場合はException（オブジェクトの型が不明です。）をスローします。<br>
	 * <br>
	 *
	 * @param targetA 操作対象情報A
	 * <br>
	 * @since Ver6.2
	 * <br>
	 */
	public void createWithDetailWriteInPathFromFolderDomainIdToSysRecBox(List<?> targetA) throws Exception {
		
		// システムごみ箱への移動時のみ処理する
		for (int i = 0; i < targetA.size(); i++) {
			boolean isParentWsRecycleBox = AppDocumentUtil.isObjectInWsRecycle((ObjectDomain) targetA.get(i));
			if (!isParentWsRecycleBox) {
				return;
			}
		}
		
		List<FolderDomain> folderDomainList = (List<FolderDomain>)targetA;
		List<String> pathList = new ArrayList<String>();
		Iterator<FolderDomain> ite = folderDomainList.iterator();
		FolderDomain folderDomain = null;
		List<ObjectDomain> folderList = new ArrayList<ObjectDomain>();
		while(ite.hasNext()){
			folderDomain = ite.next();
			String path = "";
			if( folderDomain != null && folderDomain.getId() > 0){
				ObjectDomain object = objectService.getById(folderDomain.getId());
				AttributeDomain pathAtributeDomain = object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_PASS"));
				if(object != null && pathAtributeDomain != null){
					path = pathAtributeDomain.getStringList().get(0);
				}
			}
			pathList.add(path);
			folderList.add((ObjectDomain)folderDomain);
		}
		
		super.createWithDetail(targetA, pathList);
	}
	
	/**
	 * 操作履歴出力に使用するオブジェクトServiceを取得します。
	 * @return 操作履歴出力に使用するオブジェクトService
	 */
	public ObjectService getObjectService() {
	    return objectService;
	}

	/**
	 * 操作履歴出力に使用するオブジェクトServiceを設定します。
	 * @param operationHistoryService 操作履歴出力に使用するオブジェクトService
	 */
	public void setObjectService(ObjectService objectService) {
	    this.objectService = objectService;
	}
}