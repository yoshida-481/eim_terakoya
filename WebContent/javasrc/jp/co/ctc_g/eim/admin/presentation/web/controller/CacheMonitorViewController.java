package jp.co.ctc_g.eim.admin.presentation.web.controller;

import java.util.ArrayList;
import java.util.List;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import jp.co.ctc_g.eim.admin.business.domain.CacheNodeDomain;
import jp.co.ctc_g.eim.admin.business.service.CacheMonitorViewService;
import jp.co.ctc_g.eim.admin.presentation.web.dto.CacheNodeDTO;
import jp.co.ctc_g.eim.admin.presentation.web.dto.CacheReloadDTO;
import jp.co.ctc_g.eim.admin.presentation.web.form.criteria.CacheEntrySearchCriteriaForm;
import jp.co.ctc_g.eim.framework2.business.domain.cache.CacheEntryDomain;
import jp.co.ctc_g.eim.framework2.business.domain.cache.CacheEntrySearchDomain;
import jp.co.ctc_g.eim.framework2.business.domain.cache.CacheSpaceDomain;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.presentation.web.controller.RestController;
import jp.co.ctc_g.eim.framework2.presentation.web.dto.CacheEntryDTO;
import jp.co.ctc_g.eim.framework2.presentation.web.dto.CacheEntrySearchDTO;
import jp.co.ctc_g.eim.framework2.presentation.web.dto.CacheSpaceDTO;

@Controller
@RequestMapping(value = "/rest/cache-monitor-view")
public class CacheMonitorViewController extends RestController {

	private CacheMonitorViewService cacheMonitorViewService = null;

    /**
     * ノードの一覧を返します。
	 * 本メソッドは{@link jp.co.ctc_g.eim.admin.business.service.CacheMonitorViewService#getNodes() Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.admin.business.service.CacheMonitorViewService#getNodes()
     */
	@RequestMapping(value = "/nodes", method=RequestMethod.GET)
	@ResponseBody
	public List<CacheNodeDTO> getNodes(HttpServletRequest request, HttpServletResponse response) throws Exception {

		List<CacheNodeDomain> nodeDomainList = cacheMonitorViewService.getNodes();

		// Domain ⇒ DTO 変換
		List<CacheNodeDTO> nodesDTOList = new ArrayList<CacheNodeDTO>(nodeDomainList.size());
		for (CacheNodeDomain domain : nodeDomainList) {
			CacheNodeDTO dto = new CacheNodeDTO();
			BeanUtils.copyProperties(domain, dto);
			nodesDTOList.add(dto);
		}

		return nodesDTOList;
	}

	/**
     * 指定されたノード内のキャッシュ領域の一覧を返します。
	 * 本メソッドは{@link jp.co.ctc_g.eim.admin.business.service.CacheMonitorViewService#getCaches(long nodeId) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.admin.business.service.CacheMonitorViewService#getCaches(long nodeId)
	 */
	@RequestMapping(value = "/caches", method=RequestMethod.GET)
	@ResponseBody
	public List<CacheSpaceDTO> getCaches(@RequestParam("nodeId") long nodeId, HttpServletRequest request, HttpServletResponse response) throws Exception {

		List<CacheSpaceDomain> spaceDomainList = cacheMonitorViewService.getCaches(nodeId);

		// Domain ⇒ DTO 変換
		List<CacheSpaceDTO> spaceDTOList = new ArrayList<CacheSpaceDTO>(spaceDomainList.size());
		for (CacheSpaceDomain domain : spaceDomainList) {
			CacheSpaceDTO dto = new CacheSpaceDTO();
			BeanUtils.copyProperties(domain, dto);
			spaceDTOList.add(dto);
		}

		return spaceDTOList;
	}

	/**
     * 指定されたIDに一致するエントリの一覧を返します。
	 * 本メソッドは{@link jp.co.ctc_g.eim.admin.business.service.CacheMonitorViewService#searchEntries(CacheEntrySearchCriteriaForm criteriaForm) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.admin.business.service.CacheMonitorViewService#searchEntries(CacheEntrySearchCriteriaForm criteriaForm)
	 */
	@RequestMapping(value = "/entries/search", method=RequestMethod.POST)
	@ResponseBody
	public CacheEntrySearchDTO searchEntries(@RequestBody jp.co.ctc_g.eim.admin.presentation.web.form.criteria.CacheEntrySearchCriteriaForm criteriaForm, HttpServletRequest request, HttpServletResponse response) throws Exception {

		// DTO ⇒ Domain 変換
		jp.co.ctc_g.eim.admin.business.domain.criteria.CacheEntrySearchCriteria criteriaDomain = new jp.co.ctc_g.eim.admin.business.domain.criteria.CacheEntrySearchCriteria();
		criteriaDomain.setMaxCount(Integer.parseInt(ConfigUtils.getByKey("SEARCH_CACHE_ENTRY_RESULT_MAX")));
		criteriaDomain.setNodeId(criteriaForm.getNodeId());
		criteriaDomain.setCacheSpaceKey(criteriaForm.getCacheSpaceKey());
		criteriaDomain.setIdList(criteriaForm.getIdList());

		CacheEntrySearchDomain entrySearchDomain = cacheMonitorViewService.searchEntries(criteriaDomain);

		// Domain ⇒ DTO 変換
		CacheEntrySearchDTO entrySearchDTO = new CacheEntrySearchDTO();
		List<CacheEntryDTO> entryDTOList = new ArrayList<CacheEntryDTO>();
		// コピー処理
		entrySearchDTO.setAllCounts(entrySearchDomain.getAllCounts());
		entrySearchDTO.setAttributeTypeList(entrySearchDomain.getAttributeTypeList());
		for (CacheEntryDomain domain : entrySearchDomain.getEntryList()) {
			CacheEntryDTO dto = new CacheEntryDTO();
			dto.setReloading(domain.isReloading());
			dto.setEntry(domain.getEntry());
			dto.setAttributeValueMap(domain.getAttributeValueMap());
			BeanUtils.copyProperties(domain, dto);
			entryDTOList.add(dto);
		}
		entrySearchDTO.setEntryList(entryDTOList);

		return entrySearchDTO;
	}

	/**
     * 指定されたIDに一致するエントリを更新ログに書き込みます。
	 * 本メソッドは{@link jp.co.ctc_g.eim.admin.business.service.CacheMonitorViewService#reload(CacheReloadDTO reloadDTO) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.admin.business.service.CacheMonitorViewService#reload(CacheReloadDTO reloadDTO)
	 */
	@RequestMapping(value = "/reload", method=RequestMethod.POST)
	@ResponseBody
	public void reload(@RequestBody CacheReloadDTO reloadDTO, HttpServletRequest request, HttpServletResponse response) throws Exception {

		cacheMonitorViewService.reload(reloadDTO.getCacheSpaceKey(), reloadDTO.getPkList());
	}

	/**
	 * キャッシュモニタービューサービスを取得します。
	 * @return キャッシュモニタービューサービス
	 */
	public CacheMonitorViewService getCacheMonitorViewService() {
		return cacheMonitorViewService;
	}

	/**
	 * キャッシュモニタービューサービスを設定します。
	 * @param cacheMonitorViewService キャッシュモニタービューサービス
	 */
	public void setCacheMonitorViewService(CacheMonitorViewService cacheMonitorViewService) {
		this.cacheMonitorViewService = cacheMonitorViewService;
	}

}
