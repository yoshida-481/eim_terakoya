package jp.co.ctc_g.eim.app.document.common.aop.advice;

import jp.co.ctc_g.eim.app.document.business.domain.DocumentDomain;
import jp.co.ctc_g.eim.app.document.business.domain.FolderDomain;
import jp.co.ctc_g.eim.app.document.business.domain.WorkspaceDomain;
import jp.co.ctc_g.eim.app.document.common.util.AppDocumentUtil;
import jp.co.ctc_g.eim.framework2.business.domain.FileAccessDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.common.aop.advice.OperationHistoryAdvice;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;

public class DocumentOperationHistoryAdvice extends OperationHistoryAdvice {
	
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
	public void createWithDetailWriteInPathFromFolderDomain(Object targetA) throws Exception {
		FolderDomain folderDomain = (FolderDomain)targetA;
		String path = "";
		if( folderDomain != null && folderDomain.getPath() != null){
			path = folderDomain.getPath();
		}
		
		super.createWithDetail(targetA, path);
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
	public void createWithDetailWriteInPathFromFolderDomainID(Object targetA,Object targetPlace) throws Exception {
		
		boolean isParentRecycleBox = AppDocumentUtil.isObjectInRecycleWithoutRecycle((ObjectDomain) targetA);
		if (isParentRecycleBox) {
			// ごみ箱からの移動時は処理しない
			return;
		}

		WorkspaceDomain placeDomain = (WorkspaceDomain)targetPlace;
		String path = "";
		if( placeDomain != null && placeDomain.getId() > 0){
			ObjectDomain object = objectService.getById(placeDomain.getId());
			AttributeDomain pathAtributeDomain = object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_PASS"));
			if(object != null){
				if(pathAtributeDomain != null){
					path = pathAtributeDomain.getStringList().get(0) + object.getName() + "/";
				}else{
					path = "/" + object.getName() + "/";
				}
			}
		}
		super.createWithDetail(targetA, path);
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
	public void createWithDetailWriteInPathFromFolderDomainIdForBackFromWsRecBox(Object targetA,Object targetPlace) throws Exception {
		
		boolean isObjectInWsRecycle = AppDocumentUtil.isObjectInWsRecycle((ObjectDomain) targetA);
		// ワークスペースごみ箱からの移動時のみ処理する
		if (!isObjectInWsRecycle) {
			return;
		}
		
		WorkspaceDomain placeDomain = (WorkspaceDomain)targetPlace;
		String path = "";
		if( placeDomain != null && placeDomain.getId() > 0){
			ObjectDomain object = objectService.getById(placeDomain.getId());
			AttributeDomain pathAtributeDomain = object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_PASS"));
			if(object != null){
				if(pathAtributeDomain != null){
					path = pathAtributeDomain.getStringList().get(0) + object.getName() + "/";
				}else{
					path = "/" + object.getName() + "/";
				}
			}
		}

		super.createWithDetail(targetA, path);
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
	public void createWithDetailWriteInPathFromFolderDomainIdForBackFromSysRecBox(Object targetA,Object targetPlace) throws Exception {
		
		boolean isObjectInSysRecycle = AppDocumentUtil.isObjectInSysRecycle((ObjectDomain) targetA);
		// システムごみ箱からの移動時のみ処理する
		if (!isObjectInSysRecycle) {
			return;
		}
		
		WorkspaceDomain placeDomain = (WorkspaceDomain)targetPlace;
		String path = "";
		if( placeDomain != null && placeDomain.getId() > 0){
			ObjectDomain object = objectService.getById(placeDomain.getId());
			AttributeDomain pathAtributeDomain = object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_PASS"));
			if(object != null){
				if(pathAtributeDomain != null){
					path = pathAtributeDomain.getStringList().get(0) + object.getName() + "/";
				}else{
					path = "/" + object.getName() + "/";
				}
			}
		}

		super.createWithDetail(targetA, path);
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
	public void createWithDetailWriteInPathFromDocumentDomain(Object targetA) throws Exception {
		DocumentDomain documentDomain = (DocumentDomain)targetA;
		String path = "";
		if( documentDomain != null && documentDomain.getPath() != null){
			path = documentDomain.getPath();
		}
		
		super.createWithDetail(targetA, path);
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
	public void createWithDetailWriteInPathFromDocumentDomainID(Object targetA) throws Exception {
		DocumentDomain documentDomain = (DocumentDomain)targetA;
		String path = "";
		if( documentDomain != null && documentDomain.getId() > 0){
			ObjectDomain object = objectService.getById(documentDomain.getId());
			AttributeDomain pathAtributeDomain = object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_PASS"));
			if(object != null && pathAtributeDomain != null){
				path = pathAtributeDomain.getStringList().get(0);
			}
		}
		
		super.createWithDetail(targetA, path);
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
	public void createWithDetailWriteInPathForFileAccessDomain(Object targetA) throws Exception {
		FileAccessDomain fileAccessDomain = (FileAccessDomain)targetA;
		String path = "";
		
		if( fileAccessDomain != null && fileAccessDomain.getObject().getId() > 0){
			ObjectDomain object = objectService.getById(fileAccessDomain.getObject().getId());
			AttributeDomain pathAtributeDomain = object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_PASS"));
			if(object != null && pathAtributeDomain != null){
				path = pathAtributeDomain.getStringList().get(0);
			}
		}
		
		super.createWithDetail(fileAccessDomain.getObject(), path);
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
	public void createWithDetailWriteInPathForMoveDocument(Object targetA,Object targetPlace) throws Exception {
		
		boolean isParentRecycleBox = AppDocumentUtil.isObjectInRecycleWithoutRecycle((ObjectDomain) targetA);
		if (isParentRecycleBox) {
			// ごみ箱からの移動時は処理しない
			return;
		}
		
		WorkspaceDomain placeDomain = (WorkspaceDomain)targetPlace;
		String path = "";
		if( placeDomain != null && placeDomain.getId() > 0){
			ObjectDomain object = objectService.getById(placeDomain.getId());
			AttributeDomain pathAtributeDomain = object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_PASS"));
			if(object != null){
				if(pathAtributeDomain != null){
					path = pathAtributeDomain.getStringList().get(0) + object.getName() + "/";
				}else{
					path = "/" + object.getName() + "/";
				}
			}
		}
		
		super.createWithDetail(targetA, path);
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
	public void createWithDetailWriteInPathForMoveDocumentForBackFromWsRecBox(Object targetA,Object targetPlace) throws Exception {
		
		boolean isObjectInWsRecycle = AppDocumentUtil.isObjectInWsRecycle((ObjectDomain) targetA);
		// ワークスペースごみ箱からの移動時のみ処理する
		if (!isObjectInWsRecycle) {
			return;
		}
		
		WorkspaceDomain placeDomain = (WorkspaceDomain)targetPlace;
		String path = "";
		if( placeDomain != null && placeDomain.getId() > 0){
			ObjectDomain object = objectService.getById(placeDomain.getId());
			AttributeDomain pathAtributeDomain = object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_PASS"));
			if(object != null){
				if(pathAtributeDomain != null){
					path = pathAtributeDomain.getStringList().get(0) + object.getName() + "/";
				}else{
					path = "/" + object.getName() + "/";
				}
			}
		}
		
		super.createWithDetail(targetA, path);
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
	public void createWithDetailWriteInPathForMoveDocumentForBackFromSysRecBox(Object targetA,Object targetPlace) throws Exception {
		
		boolean isObjectInSysRecycle = AppDocumentUtil.isObjectInSysRecycle((ObjectDomain) targetA);
		// システムごみ箱からの移動時のみ処理する
		if (!isObjectInSysRecycle) {
			return;
		}
		
		WorkspaceDomain placeDomain = (WorkspaceDomain)targetPlace;
		String path = "";
		if( placeDomain != null && placeDomain.getId() > 0){
			ObjectDomain object = objectService.getById(placeDomain.getId());
			AttributeDomain pathAtributeDomain = object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_PASS"));
			if(object != null){
				if(pathAtributeDomain != null){
					path = pathAtributeDomain.getStringList().get(0) + object.getName() + "/";
				}else{
					path = "/" + object.getName() + "/";
				}
			}
		}
		
		super.createWithDetail(targetA, path);
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