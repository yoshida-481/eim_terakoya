package jp.co.ctc_g.eim.app.document.business.service;

import java.util.List;

import jakarta.servlet.http.HttpServletRequest;

import jp.co.ctc_g.eim.app.document.business.domain.CirculationSearchDomain;

/**
 * 回付状況一覧検索に関する操作を行うビジネスサービスです。
 * @since Ver 6,6
 */
public interface CirculationSearchService {

	/**
	 * 回付状況一覧を検索します。<br>
	 * @param request 検索条件
	 * @return 検索結果
	 * @throw Exception
	 */
	public List<CirculationSearchDomain> search(HttpServletRequest request) throws Exception;
}
