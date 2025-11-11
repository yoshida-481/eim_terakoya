package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.List;

import jp.co.ctc_g.eim.app.form.business.domain.FileObjectCreatorDomain;
import jp.co.ctc_g.eim.app.form.business.service.FileObjectCreatorService;
import jp.co.ctc_g.eim.app.form.business.service.FormAttachedFileService;
import jp.co.ctc_g.eim.app.form.business.service.impl.FileObjectCreatorServiceImpl;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessHistoryDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.service.AccessHistoryService;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;

/**
 * WebDAV通信の為の付加的な機能を提供するサービス実装クラス
 * @see jp.co.ctc_g.eim.app.form.business.service.FileObjectCreatorService
 */
public class DocumentFileObjectCreatorServiceImpl extends FileObjectCreatorServiceImpl implements FileObjectCreatorService {
	
	/** アクセス履歴サービス */
	private AccessHistoryService accessHistoryService;
	/** 帳票添付ファイルサービス */
	private FormAttachedFileService formAttachedFileService;
	
	
	/**
	 * @see jp.co.ctc_g.eim.app.form.business.service.FileObjectCreatorService#copy(FileObjectCreatorDomain fileObjectCreatorDomain)
	 */
	@Override
	public List<FileObjectCreatorDomain> copy(FileObjectCreatorDomain fileObjectCreatorDomain) throws Exception{
		
		// WebDAV編集時は、帳票添付ファイルからコピーした一時添付ファイルオブジェクトを対象とするため、
		// アクセス履歴登録、操作履歴登録、アクセス権限チェックで必要となる添付元オブジェクトを取得することができない。
		// そのため、WebDAV編集時のアクセス履歴登録、操作履歴登録、アクセス権限チェックは本メソッドで実施する。
		// 操作履歴登録、アクセス権限チェックはAOPにて処理を実施する。
		
		
		List<FileObjectCreatorDomain> objList = super.copy(fileObjectCreatorDomain);
		
		// 添付元文書取得
		ObjectDomain document = formAttachedFileService.getParentByFileId(fileObjectCreatorDomain.getId());
		
		if(document != null) {
			// 添付ファイル取得
			ObjectDomain attachedFile = super.getObjectService().getById(fileObjectCreatorDomain.getId());
			
			// アクセス履歴(ダウンロード)登録
			AccessHistoryDomain accessHistory = new AccessHistoryDomain();
			accessHistory.setAction("EIM.ACCESS.TYPE.ATTATCHED.FILE.DOWNLOAD" + ConfigUtils.getByKey("ACCESS_HISTORY_DELIMITER") + attachedFile.getName());
			accessHistoryService.create(document, accessHistory);
		}
		
		return objList;
	}
	
	
	/**
	 * アクセス履歴サービスを取得します。
	 * @return アクセス履歴サービス
	 */
	public AccessHistoryService getAccessHistoryService() {
		return accessHistoryService;
	}

	/**
	 * アクセス履歴サービスを設定します。
	 * @param accessHistoryService アクセス履歴サービス
	 */
	public void setAccessHistoryService(AccessHistoryService accessHistoryService) {
		this.accessHistoryService = accessHistoryService;
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
