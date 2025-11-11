package common.tools.internal;

import java.util.ArrayList;
import java.util.List;
import common.tools.*;
import eim.bo.EIMObject;
/**
 * パスの構成情報を表すドメイン
 */
public class PathDomain {

	/**
	 * リンクのタイプ
	 */
	private PathTypeEnum type = null;
	
	/**
	 * リレーションからたどったパス情報
	 */
	private String realPath = "";
	
	/**
	 * PathDomainの一番最下層のオブジェクト（latestであれば、objectPathの際下層部と同じ、latestでなければ、対象のオブジェクトそのものが入る)<br>
	 */
	private EIMObject leafObject = null;
	
	/**
	 * ルートノードから対象ノードまでのオブジェクトのリスト
	 * (最新でないオブジェクトの場合は、最新のオブジェクトのリストが入る)
	 */
	private List<EIMObject> objectPath = new ArrayList<EIMObject>();

	/**
	 * リンクのタイプを取得します。
	 * @return リンクのタイプ
	 */
	public PathTypeEnum getType() {
	    return type;
	}

	/**
	 * リンクのタイプを設定します。
	 * @param type リンクのタイプ
	 */
	public void setType(PathTypeEnum type) {
	    this.type = type;
	}

	/**
	 * リレーションからたどったパス情報を取得します。
	 * @return リレーションからたどったパス情報
	 */
	public String getRealPath() {
	    return realPath;
	}

	/**
	 * リレーションからたどったパス情報を設定します。
	 * @param realPath リレーションからたどったパス情報
	 */
	public void setRealPath(String realPath) {
	    this.realPath = realPath;
	}


	/**
	 * PathDomainの一番最下層のオブジェクト（latestであれば、objectPathの際下層部と同じ、latestでなければ、対象のオブジェクトそのものが入る)<br>を取得します。
	 * @return PathDomainの一番最下層のオブジェクト（latestであれば、objectPathの際下層部と同じ、latestでなければ、対象のオブジェクトそのものが入る)<br>
	 */
	public EIMObject getLeafObject() {
	    return leafObject;
	}

	/**
	 * PathDomainの一番最下層のオブジェクト（latestであれば、objectPathの際下層部と同じ、latestでなければ、対象のオブジェクトそのものが入る)<br>を設定します。
	 * @param leafObject PathDomainの一番最下層のオブジェクト（latestであれば、objectPathの際下層部と同じ、latestでなければ、対象のオブジェクトそのものが入る)<br>
	 */
	public void setLeafObject(EIMObject leafObject) {
	    this.leafObject = leafObject;
	}

	/**
	 * ルートノードから対象ノードまでのオブジェクトのリストを取得します。
	 * @return ルートノードから対象ノードまでのオブジェクトのリスト
	 */
	public List<EIMObject> getObjectPath() {
	    return objectPath;
	}

	/**
	 * ルートノードから対象ノードまでのオブジェクトのリストを設定します。
	 * @param objectPath ルートノードから対象ノードまでのオブジェクトのリスト
	 */
	public void setObjectPath(List<EIMObject> objectPath) {
	    this.objectPath = objectPath;
	}
	
	
}
