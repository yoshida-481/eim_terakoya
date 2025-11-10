package jp.co.ctc_g.eim.app.document.common.aop.advice;

import java.util.Date;

import jp.co.ctc_g.eim.framework2.business.domain.box.BoxFileCopyFromEIMFileDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessHistoryDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.business.service.AccessHistoryService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.common.aop.plugin.ParameterConverterPlugIn;
import jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext;

/**
 * アクセス履歴登録のためのアドバイスクラスです.
 * <p>
 * アプリケーションコンテキスト設定における、AOPのアドバイスbeanとして宣言し、
 * サービスファサードへのAOPとして適用してください。
 * </p>
 * 
 * @since 1.0
 */
public class DocumentAccessHistoryAdvice  implements ParameterConverterPlugIn {

	/** アクセス履歴サービス */
	private AccessHistoryService accessHistoryService;

	/** アクセス内容 */
	private String accessType;
	/**
	 * パラメータコンバータ
	 */
	private ParameterConverterPlugIn parameterConverter;
	
	/** オブジェクトサービス */
	private ObjectService objectService;
	

	/**
	 * オブジェクトドメインに関するアクセス履歴の登録を行います.
	 * 
	 * @param o オブジェクトドメイン
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 * @since 1.0
	 */
	public void registerAboutObject(Object o) throws Exception {
		createHistory((ObjectDomain) convert(o));
	}
	
	public Object convert(Object o) throws Exception {
		// パラメータ変換
		if (parameterConverter != null) {
			return (ObjectDomain)parameterConverter.convert(o);
		} else if(o instanceof BoxFileCopyFromEIMFileDomain) {
			BoxFileCopyFromEIMFileDomain boxFileObject = (BoxFileCopyFromEIMFileDomain) o;
			return objectService.getById( boxFileObject.getObjectId());
		}
		else{
			return (ObjectDomain)o;
		}
	}

	private void createHistory(ObjectDomain o) throws Exception {
		AccessHistoryDomain history = new AccessHistoryDomain();
		history.setUser(getUser());
		history.setDate(new Date());
		history.setAction(accessType);

		accessHistoryService.create(o, history);
	}

	private UserDomain getUser() {
		return EIMThreadContext.getTransactionContext().getUser();
	}

	/**
	 * 使用する操作履歴サービスを設定します.
	 * 
	 * @param accessHistoryService 操作履歴サービス
	 * @since 1.0
	 */
	public void setAccessHistoryService(AccessHistoryService accessHistoryService) {
		this.accessHistoryService = accessHistoryService;
	}
	
	/**
	 * 使用するオブジェクトサービスを設定します.
	 *
	 * @param objectService オブジェクトサービス
	 * @since 1.0
	 */
	public void setObjectService(ObjectService objectService) {
		this.objectService = objectService;
	}

	/**
	 * アクセス内容を設定します.
	 * 
	 * @param accessType アクセス内容
	 * @since 1.0
	 */
	public void setAccessType(String accessType) {
		this.accessType = accessType;
	}

	/**
	 * パラメータコンバータを取得します。
	 * @return パラメータコンバータ
	 */
	public ParameterConverterPlugIn getParameterConverter() {
	    return parameterConverter;
	}

	/**
	 * パラメータコンバータを設定します。
	 * @param parameterConverter パラメータコンバータ
	 */
	public void setParameterConverter(ParameterConverterPlugIn parameterConverter) {
	    this.parameterConverter = parameterConverter;
	}
}