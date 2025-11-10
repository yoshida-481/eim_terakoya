package jp.co.ctc_g.eim.app.document.business.service;


import java.io.OutputStream;
import java.util.List;
import java.util.Map;

import jakarta.servlet.http.HttpServletRequest;

import jp.co.ctc_g.eim.app.document.presentation.dto.BoxDocumentCreateDTO;
import jp.co.ctc_g.eim.app.document.presentation.dto.ConfirmBoxDocumentDTO;
import jp.co.ctc_g.eim.framework2.business.domain.box.BoxFileDomain;

/**
 * Boxのファイルを操作するための機能を提供するサービスです。
 *
 * @since Ver6.44
 */
public interface BoxDocumentService {

	/**
	 * BoxのファイルIDを指定してファイル情報を取得します。
     * 本メソッドは{@link jp.co.ctc_g.eim.framework2.business.dao.box.BoxFileDao#getById(String) DAO}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.framework2.business.dao.box.BoxFileDao#getById(String) DAO
	 * @see jp.co.ctc_g.eim.framework2.business.service.box.impl.BoxFileServiceImpl#getById(String)
	 * @since Ver6.44
	 */
	BoxFileDomain getById(String id) throws Exception;

	/**
	 * Boxのファイルを出力ストリームに書き出します。
     * 本メソッドは{@link jp.co.ctc_g.eim.framework2.business.dao.box.BoxFileDao#download(String, OutputStream) DAO}へ処理を委譲します。
	 * @return
	 * @see jp.co.ctc_g.eim.framework2.business.dao.box.BoxFileDao#download(String, OutputStream) DAO
	 * @see jp.co.ctc_g.eim.framework2.business.service.box.impl.BoxFileServiceImpl#download(String, OutputStream)
	 * @since Ver6.44
	 */
	void download(String id, OutputStream stream) throws Exception;

	/**
	 * BoxのファイルIDを指定してファイル情報を削除します。
	 * @return BoxのファイルID
	 */
	void delete(String id,String type,String name,String path) throws Exception;

	/**
	 * BoxからEIM上へドキュメントの登録可否確認します。
	 * @return ファイル登録可否結果
	 * @see jp.co.ctc_g.eim.app.document.business.service.BoxDocumentService#parentCheck(Map<String, Object> files,HttpServletRequest request)
	 */
	List<ConfirmBoxDocumentDTO> confirmBoxDocument(Map<String, Object> files,HttpServletRequest request)throws Exception;

	/**
	 * 一時格納オブジェクトを作成します。
	 * @return オブジェクトID
	 * @throws Exception
	 */
	Long createObject(List<Map<String, Object>> files, HttpServletRequest request) throws Exception;

	/**
	 * ファイルの登録をします。
	 * @return ファイル登録情報
	 * @see jp.co.ctc_g.eim.app.document.business.service.BoxDocumentService#create(Map<String, Object> files,HttpServletRequest request)
	 */
	List<BoxDocumentCreateDTO> createDocument(Map<String, Object> createFile, HttpServletRequest request) throws Exception;

	/**
	 * 一時格納フォルダを削除します。
	 * @param resquest HTTPサーブレットリクエスト
	 */
	void deleteTmpFolder(HttpServletRequest request);

}
