package jp.co.ctc_g.eim.admin.integration.dao.impl;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Types;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jp.co.ctc_g.eim.admin.business.dao.AdminUserDao;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.dao.UserDao;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.MultipleCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.UserCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.OtherNameDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;
import jp.co.ctc_g.eim.framework2.integration.dao.plugin.DatabasePlugInLoader;

/**
 * システム管理のユーザ操作に関する機能のDAOクラス
 * @see jp.co.ctc_g.eim.admin.business.dao.AdminUserDao
 */
public class AdminUserDaoImpl implements AdminUserDao {

	/** ユーザDAO */
	private UserDao userDao;

	/**
	 * 指定したユーザの他言語名称を取得し、ユーザIDをキーとしたマップを返却します。
	 *
	 * @see jp.co.ctc_g.eim.admin.business.dao.AdminUserDao#getOtherNameListMapByIds(List<UserDomain>)
	 */
	public Map<Long, List<OtherNameDomain>> getOtherNameListMapByIds(List<UserDomain> userDomainList) throws Exception {
		TransactionContext tx = EIMThreadContext.getTransactionContext();
		Map<Long, List<OtherNameDomain>> otherNameListMap = new HashMap<>();

		List<List<Long>> userIdsList = new ArrayList<>();
		List<Long> userIdList = new ArrayList<>();
		int cnt = 0;
		for(UserDomain userDomain: userDomainList) {
			++cnt;

			userIdList.add(userDomain.getId());

			// IDを1000件ずつに分割
			if(cnt == 1000) {
				cnt = 0;
				userIdsList.add(userIdList);
				userIdList = new ArrayList<>();
			}
		}

		if(userIdList.size() > 0) {
			userIdsList.add(userIdList);
		}

		PreparedStatement pstmt = null;
		ResultSet rset = null;
		try {
			Connection conn = tx.getDBConnection();

			String sql = "select usid, lid, name from eimuserother where ";
			for(int i = 0; i < userIdsList.size(); i++) {
				if(i == 0) {
					sql += String.format("usid in (%s)", DatabasePlugInLoader.getPlugIn().getQueryStringSelectArray(Types.BIGINT));
				} else {
					sql += String.format(" or usid in (%s)", DatabasePlugInLoader.getPlugIn().getQueryStringSelectArray(Types.BIGINT));
				}
			}

			sql += " order by usid, lid";

			pstmt = conn.prepareStatement(sql);
			for(int i = 0; i < userIdsList.size(); i++) {
				pstmt.setArray(i + 1, DatabasePlugInLoader.getPlugIn().createArray(conn, userIdsList.get(i).toArray(new Long[0])));
			}

			rset = pstmt.executeQuery();

			while (rset.next()) {
				long userId = rset.getLong("usid");
				OtherNameDomain otherName = new OtherNameDomain();
                otherName.setLangId(rset.getString("lid"));
                otherName.setName(rset.getString("name"));
                if(otherNameListMap.containsKey(Long.valueOf(userId))) {
                	otherNameListMap.get(Long.valueOf(userId)).add(otherName);
                } else {
                	List<OtherNameDomain> otherNameList = new ArrayList<>();
                	otherNameList.add(otherName);
                	otherNameListMap.put(userId, otherNameList);
                }

			}

			return otherNameListMap;

		} finally {
			if (rset != null) {
				rset.close();
			}
			if (pstmt != null) {
				pstmt.close();
			}
		}
	}

	/**
	 * 指定したユーザの他言語名称を取得し、ユーザIDをキーとしたマップを返却します。
	 *
	 * @see jp.co.ctc_g.eim.admin.business.dao.AdminUserDao#getListByCodes(List<String>)
	 */
	public Map<String, UserDomain> getListByCodes(List<String> userCodeList) throws Exception {
		TransactionContext tx = EIMThreadContext.getTransactionContext();
		Map<String, UserDomain> userMap = new HashMap<>();

		List<List<String>> userCodes = new ArrayList<>();
		List<String> codeList = new ArrayList<>();
		int cnt = 0;
		for(String userCode: userCodeList) {
			++cnt;

			codeList.add(userCode);

			// IDを1000件ずつに分割
			if(cnt == 1000) {
				cnt = 0;
				userCodes.add(codeList);
				codeList = new ArrayList<>();
			}
		}

		if(codeList.size() > 0) {
			userCodes.add(codeList);
		}

		PreparedStatement pstmt = null;
		ResultSet rset = null;
		MultipleCriteria<Long> userIdList = new MultipleCriteria<>();
		try {
			Connection conn = tx.getDBConnection();

			String sql = "select id from eimuser where ";
			for(int i = 0; i < userCodes.size(); i++) {
				if(i == 0) {
					sql += String.format("code in (%s)", DatabasePlugInLoader.getPlugIn().getQueryStringSelectArray(Types.VARCHAR));
				} else {
					sql += String.format(" or code in (%s)", DatabasePlugInLoader.getPlugIn().getQueryStringSelectArray(Types.VARCHAR));
				}
			}

			pstmt = conn.prepareStatement(sql);
			for(int i = 0; i < userCodes.size(); i++) {
				pstmt.setArray(i + 1, DatabasePlugInLoader.getPlugIn().createArray(conn, userCodes.get(i).toArray(new String[0])));
			}

			rset = pstmt.executeQuery();

			while (rset.next()) {
				long userId = rset.getLong("id");
				userIdList.add(userId);
			}

		} finally {
			if (rset != null) {
				rset.close();
			}
			if (pstmt != null) {
				pstmt.close();
			}
		}

		if(userIdList.size() > 0) {
			UserCriteria criteria = new UserCriteria();
			criteria.setIds(userIdList);
			List<UserDomain> userList = userDao.getList(criteria);

			for(UserDomain userDomain: userList) {
				userMap.put(userDomain.getCode(), userDomain);
			}
		}

		return userMap;
	}

	/**
	 * ユーザDAOを取得します。
	 * @return ユーザDAO
	 */
	public UserDao getUserDao() {
		return userDao;
	}

	/**
	 * ユーザDAOを設定します。
	 * @param userDao ユーザDAO
	 */
	public void setUserDao(UserDao userDao) {
		this.userDao = userDao;
	}
}
