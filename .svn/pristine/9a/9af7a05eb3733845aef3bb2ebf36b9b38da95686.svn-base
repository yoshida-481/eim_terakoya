package jp.co.ctc_g.eim.app.document.business.service;

import java.util.List;
import java.util.Set;

import org.springframework.web.multipart.MultipartFile;

import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService;

/**
 * PDF変換オブジェクトを作成するサービスクラスです。
 */
public interface PublicDocumentService {

	/**
	 * PDF変換オブジェクトを作成します<br>
	 *
	 * @param objdomain オブジェクトドメイン
	 * @throws Exception 例外処理
	 * @since Ver 6.13
	 */
	public void createAsync(ObjectDomain object) throws Exception;

	/**
	 * 指定されたオブジェクトIDリストの内、PDF変換中のオブジェクトIDをSetで返却します.
	 * @param ids PDF変換中か確認するオブジェクトのIDリスト
	 * @throws Exception 例外処理
	 * @since Ver 6.13
	 */
	public Set<Long> getPDFConversionProcessingObjectIdSet(List<Long>idList) throws Exception;

	/**
	 * 公開PDFを事前登録します<br>
	 *
	 * @param objId オブジェクトID
	 * @param file アップロードファイル
	 * @throws Exception 例外処理
	 * @since Ver 6.16
	 */
	public void preRegistPublicPdf(ObjectDomain object, MultipartFile file) throws Exception;

	/**
	 * 事前登録した公開PDFを削除します<br>
	 *
	 * @param objId オブジェクトID
	 * @throws Exception 例外処理
	 * @since Ver 6.16
	 */
	public void deletePreRegistPdf(ObjectDomain object) throws Exception;

	/**
	 * @param objectService セットします objectService
	 */
	public void setObjectService(ObjectService objectService);

	/**
	 * @return objectService
	 */
	public ObjectService getObjectService();

	/**
	 * @return objectTypeService
	 */
	public ObjectTypeService getObjectTypeService();


	/**
	 * @param objectTypeService セットします objectTypeService
	 */
	public void setObjectTypeService(ObjectTypeService objectTypeService);
}
