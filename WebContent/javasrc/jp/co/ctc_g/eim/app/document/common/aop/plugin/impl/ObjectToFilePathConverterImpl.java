package jp.co.ctc_g.eim.app.document.common.aop.plugin.impl;

import jp.co.ctc_g.eim.app.form.business.service.FormAttachedFileService;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.common.aop.plugin.ParameterConverterPlugIn;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;

/**
 * パラメータ変換プラグインの実装クラス
 * <br>
 * 入力値として添付ファイルを指し示すObjectDomainを受け取り、それが添付されているドキュメントのファイルパスを取得するプラグインクラスです。
 * <br>
 */
public class ObjectToFilePathConverterImpl implements ParameterConverterPlugIn {
	
	/** 帳票添付ファイルサービス */
	private FormAttachedFileService formAttachedFileService;
	
	/**
	 * パラメータ変換の処理。<br>
	 * <br>
	 * ObjectDomainを受け取り、それが添付されているドキュメントのファイルパスを返却します。
	 * <br>
	 * @param source FileAccessDomainです。
	 * @return パス属性の値+オブジェクト名
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 * @since Ver6.6
	 */
	public Object convert(Object source) throws Exception {
		
		ObjectDomain obj = (ObjectDomain)source;
		
		// ファイルのオブジェクトタイプ定義名称
		String fileObjTypeDefName = obj.getType().getDefinitionName();
		
		if(fileObjTypeDefName.equals(ConfigUtils.getByKey("OBJECT_TYPE_NAME_TEMP_ATTACH_FILE"))) {
			
			// 一時添付ファイルの場合、固定文字を返却
			return ConfigUtils.getByKey("OPERATION_HISTORY_DETAIL_TEMP_ATTACHED_FILE");
			
		} else if(fileObjTypeDefName.equals(ConfigUtils.getByKey("OBJECT_TYPE_NAME_FORM_ATTACH_FILE"))) {
			
			// 帳票添付ファイルの場合、添付元オブジェクトのパス属性を返却
			long fileObjId = obj.getId();
			ObjectDomain parentObj = formAttachedFileService.getParentByFileId(fileObjId);
			String path = parentObj.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS")).getString();
			return path + parentObj.getName();
		}
		
		return "";
	}
	
	
	
	/**
	 * 帳票添付ファイルサービスを取得します。
	 * @return 帳票添付ファイルサービス
	 */
	public FormAttachedFileService getFormAttachedFileService() {
		return formAttachedFileService;
	}

	/**
	 * 帳票添付ファイルサービスを設定します。
	 * @param formAttachedFileService 帳票添付ファイルサービス
	 */
	public void setFormAttachedFileService(FormAttachedFileService formAttachedFileService) {
		this.formAttachedFileService = formAttachedFileService;
	}

}
