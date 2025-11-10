package jp.co.ctc_g.eim.app.document.common.aop.plugin.impl;

import jp.co.ctc_g.eim.app.document.business.domain.WorkspaceDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.common.aop.plugin.ParameterConverterPlugIn;

/**
 * 【ドキュメントAPI】
 * 入力値としてWorkspaceDomainを受け取り、ObjectDomainインスタンスを生成するプラグインクラスです。
 * @since Ver1.0
 */
public class WorkspaceDomainToObjectConverterPlugInImpl implements ParameterConverterPlugIn {
	/**
	 * WorkspaceDomainを受け取り、ObjectDomainインスタンスをnewして返却します。
	 * @param source WorkspaceDomainです。
	 * @return IDをコンストラクタに設定してnewしたObjectDomainインスタンスです。
	 * @since Ver1.0
	 */
	public Object convert(Object source) throws Exception {
		WorkspaceDomain domain = (WorkspaceDomain)source;
		return new ObjectDomain(domain.getId());
	}
}
