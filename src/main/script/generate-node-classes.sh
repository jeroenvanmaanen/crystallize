#!/bin/bash

set -ex

echo "Generate node classes" >&2

BASE_NAME="$1"
FQ_TREE_TRAIT="$2"

if [ -z "${BASE_NAME}" ]
then
    echo "Usage: $(basename "$0") <base-name>" >&2
    exit 1
fi

if [ -z "${FQ_TREE_TRAIT}" ]
then
    FQ_TREE_TRAIT=scala.AnyRef
fi
TREE_TRAIT="$(expr ".${FQ_TREE_TRAIT}" : '.*[.]\([^.]*\)')"
TREE_TRAIT_PACKAGE="$(expr ".${FQ_TREE_TRAIT}x" : '\.\(.*\)\.[^.]*$')"
echo "TREE_TRAIT=[${TREE_TRAIT_PACKAGE}].[${TREE_TRAIT}]" >&2

SCRIPT="$(cd "$(dirname "$0")" ; pwd)"
MAIN="$(dirname "${SCRIPT}")"
SRC="$(dirname "${MAIN}")"
BASE_PACKAGE='org.leialearns.crystallize.immutabletree'
BASE_PACKAGE_PATH="$(echo "${BASE_PACKAGE}" | tr '.' '/')"
PACKAGE="$(echo ${BASE_NAME} | tr 'A-Z' 'a-z')"
FQ_PACKAGE="${BASE_PACKAGE}.${PACKAGE}"
NODE_SRC="${MAIN}/scala/${BASE_PACKAGE_PATH}/${PACKAGE}"
echo "NODE_SRC=[${NODE_SRC}]" >&2
mkdir -p "${NODE_SRC}"

function get-kind() {
    expr "$1:" : '\([^:]*\):' || true
}
function get-pattern() {
    expr "$1::" : '[^:]*:\([^:]*\):' || true
}
function get-suffix() {
    expr "$1:::" : '[^:]*:[^:]*:\([^:]*\):' || true
}
function get-type() {
    expr "$1::::" : '[^:]*:[^:]*:[^:]*:\([^:]*\):' || true
}
(

    echo "package ${FQ_PACKAGE}"

    echo
    echo "import ${BASE_PACKAGE}._"
    if [ ".${TREE_TRAIT_PACKAGE}" != ".scala" -a ".${TREE_TRAIT_PACKAGE}" != ".${BASE_PACKAGE}" -a ".${TREE_TRAIT_PACKAGE}" != ".${FQ_PACKAGE}" ]
    then
        echo "import ${FQ_TREE_TRAIT}"
    fi

    ## echo
    ## echo "trait ${BASE_NAME}[A] extends ${TREE_TRAIT}[A] { }"

    echo
    declare -a ITEM_CASES
    declare -a BUCKET_CASES
    for LEFT in 'NONE:None' "ITEM:Some(Left(leftItem)):Item:A" "TREE:Some(Right(leftTree)):Tree:${TREE_TRAIT}"
    do
        LEFT_KIND="$(get-kind "$LEFT")"
        LEFT_PATTERN="$(get-pattern "${LEFT}")"
        LEFT_SUFFIX="$(get-suffix "$LEFT")"
        LEFT_PARAM="$(echo "${LEFT_SUFFIX}" | tr 'A-Z' 'a-z')"
        LEFT_TYPE="$(get-type "$LEFT")"
        echo "LEFT: [KIND:${LEFT_KIND}] [PATTERN:${LEFT_PATTERN}] [SUFFIX:${LEFT_SUFFIX}] [PARAM:${LEFT_PARAM}] [TYPE:${LEFT_TYPE}]" >&2
        for MIDDLE in 'ITEM:-:Item:A' "TREE:-:Bucket:${TREE_TRAIT}"
        do
            MIDDLE_KIND="$(get-kind "$MIDDLE")"
            MIDDLE_SUFFIX="$(get-suffix "$MIDDLE")"
            MIDDLE_PARAM="$(echo "${MIDDLE_SUFFIX}" | tr 'A-Z' 'a-z')"
            MIDDLE_TYPE="$(get-type "$MIDDLE")"
            echo "MIDDLE: [KIND:${MIDDLE_KIND}] [SUFFIX:${MIDDLE_SUFFIX}] [PARAM:${MIDDLE_PARAM}] [TYPE:${MIDDLE_TYPE}]" >&2
            for RIGHT in 'NONE:None' "ITEM:Some(Left(rightItem)):Item:A" "TREE:Some(Right(rightTree)):Tree:${TREE_TRAIT}"
            do
                RIGHT_KIND="$(get-kind "$RIGHT")"
                RIGHT_PATTERN="$(get-pattern "${RIGHT}")"
                RIGHT_SUFFIX="$(get-suffix "$RIGHT")"
                RIGHT_PARAM="$(echo "${RIGHT_SUFFIX}" | tr 'A-Z' 'a-z')"
                RIGHT_TYPE="$(get-type "$RIGHT")"
                echo "RIGHT: [KIND:${RIGHT_KIND}] [PATTERN:${RIGHT_PATTERN}] [SUFFIX:${RIGHT_SUFFIX}] [PARAM:${RIGHT_PARAM}] [TYPE:${RIGHT_TYPE}]" >&2
                CONSTRUCTOR='???'
                case "${LEFT_KIND}:${RIGHT_KIND}" in
                NONE:NONE)
                    CLASS_NAME="${BASE_NAME}${MIDDLE_SUFFIX}"
                    CONSTRUCTOR="${CLASS_NAME}[A](${MIDDLE_PARAM})"
                    echo "case class ${CLASS_NAME}[+A](${MIDDLE_PARAM}: ${MIDDLE_TYPE}) extends SingleNode[${MIDDLE_TYPE},A,${TREE_TRAIT}](${MIDDLE_PARAM}) with ${BASE_NAME}[A] with ${MIDDLE_SUFFIX}[A,${TREE_TRAIT}]"
                    ;;
                NONE:*)
                    NODE_CLASS="PairNode[${MIDDLE_TYPE},${RIGHT_TYPE},A,${TREE_TRAIT}]"
                    LEFT_SUFFIX="$(echo "${MIDDLE_SUFFIX}" | sed -e 's/Bucket/Tree/')"
                    CLASS_NAME="${BASE_NAME}Right${MIDDLE_SUFFIX}${RIGHT_SUFFIX}"
                    CONSTRUCTOR="${CLASS_NAME}[A](${MIDDLE_PARAM}, right${RIGHT_SUFFIX})"
                    echo "case class ${CLASS_NAME}[+A](${MIDDLE_PARAM}: ${MIDDLE_TYPE}, right${RIGHT_SUFFIX}: ${RIGHT_TYPE})"
                    echo "  extends ${NODE_CLASS}(${MIDDLE_PARAM}, right${RIGHT_SUFFIX}) with ${BASE_NAME}[A]"
                    echo "  with Right${RIGHT_SUFFIX}[A,${TREE_TRAIT}]"
                    echo "  with RightNode${MIDDLE_SUFFIX}[A,${TREE_TRAIT}]"
                    ;;
                *:NONE)
                    NODE_CLASS="PairNode[${LEFT_TYPE},${MIDDLE_TYPE},A,${TREE_TRAIT}]"
                    RIGHT_SUFFIX="$(echo "${MIDDLE_SUFFIX}" | sed -e 's/Bucket/Tree/')"
                    CLASS_NAME="${BASE_NAME}Left${LEFT_SUFFIX}${MIDDLE_SUFFIX}"
                    CONSTRUCTOR="${CLASS_NAME}[A](left${LEFT_SUFFIX}, ${MIDDLE_PARAM})"
                    echo "case class ${CLASS_NAME}[+A](left${LEFT_SUFFIX}: ${LEFT_TYPE}, ${MIDDLE_PARAM}: ${MIDDLE_TYPE})"
                    echo "  extends ${NODE_CLASS}(left${LEFT_SUFFIX}, ${MIDDLE_PARAM}) with ${BASE_NAME}[A]"
                    echo "  with LeftNode${MIDDLE_SUFFIX}[A,${TREE_TRAIT}]"
                    echo "  with Left${LEFT_SUFFIX}[A,${TREE_TRAIT}]"
                    ;;
                *)
                    NODE_CLASS="BothNodes[${LEFT_TYPE},${MIDDLE_TYPE},${RIGHT_TYPE},A,${TREE_TRAIT}]"
                    CLASS_NAME="${BASE_NAME}${LEFT_SUFFIX}${MIDDLE_SUFFIX}${RIGHT_SUFFIX}"
                    CONSTRUCTOR="${CLASS_NAME}[A](left${LEFT_SUFFIX}, ${MIDDLE_PARAM}, right${RIGHT_SUFFIX})"
                    echo "case class ${CLASS_NAME}[+A](left${LEFT_SUFFIX}: ${LEFT_TYPE}, ${MIDDLE_PARAM}: ${MIDDLE_TYPE}, right${RIGHT_SUFFIX}: ${RIGHT_TYPE})"
                    echo "  extends ${NODE_CLASS}(left${LEFT_SUFFIX}, ${MIDDLE_PARAM}, right${RIGHT_SUFFIX}) with ${BASE_NAME}[A]"
                    echo "  with Left${LEFT_SUFFIX}[A,${TREE_TRAIT}]"
                    echo "  with ${MIDDLE_SUFFIX}[A,${TREE_TRAIT}]"
                    echo "  with Right${RIGHT_SUFFIX}[A,${TREE_TRAIT}]"
                esac
                CASE_CLAUSE="case (${LEFT_PATTERN},${RIGHT_PATTERN}) => ${CONSTRUCTOR}"
                echo "CASE_CLAUSE[${MIDDLE_KIND}]: ${CASE_CLAUSE}" >&2
                case "${MIDDLE_KIND}" in
                ITEM)
                    ITEM_CASES[${#ITEM_CASES[@]}]="${CASE_CLAUSE}"
                    ;;
                *)
                    BUCKET_CASES[${#BUCKET_CASES[@]}]="${CASE_CLAUSE}"
                    ;;
                esac
            done
        done
    done
    echo ''
    echo '// Factory object'
    cat <<EOT
object ${BASE_NAME}Cases {
  def treeToEither[A,T](tree: TreeNodeTrait[A,T] with T): Either[A,TreeNodeTrait[A,T] with T] = {
    if (tree.getLeftNode.isEmpty && tree.getRightNode.isEmpty) tree.getMiddle else Right(tree)
  }
  def nodeFactory[A]: NodeFactory[A, TreeNodeTrait[A,${TREE_TRAIT}] with ${TREE_TRAIT}, Unit] = new NodeFactory[A, TreeNodeTrait[A,${TREE_TRAIT}] with ${TREE_TRAIT}, Unit] {
    def createNode(leftNodeOption: Option[TreeNodeTrait[A,${TREE_TRAIT}] with ${TREE_TRAIT}], bucket: TreeNodeTrait[A,${TREE_TRAIT}] with ${TREE_TRAIT}, rightNodeOption: Option[TreeNodeTrait[A,${TREE_TRAIT}] with ${TREE_TRAIT}], variant: Unit): TreeNodeTrait[A,${TREE_TRAIT}] with ${TREE_TRAIT} = {
      val middle: Either[A,_] = treeToEither(bucket)
      (leftNodeOption, middle, rightNodeOption) match {
        case (None, _, None) => bucket
        case (_, Left(item), _) => createNode(leftNodeOption, item, rightNodeOption, variant)
        case _ =>
          (leftNodeOption map (treeToEither(_)), rightNodeOption map (treeToEither(_))) match {
$(for CASE in "${BUCKET_CASES[@]}" ; do echo "            $CASE" ; done)
          }
      }
    }
    def createNode(leftNodeOption: Option[TreeNodeTrait[A,${TREE_TRAIT}] with ${TREE_TRAIT}], item: A, rightNodeOption: Option[TreeNodeTrait[A,${TREE_TRAIT}] with ${TREE_TRAIT}], variant: Unit): TreeNodeTrait[A,${TREE_TRAIT}] with ${TREE_TRAIT} = {
      (leftNodeOption map (treeToEither(_)), rightNodeOption map (treeToEither(_))) match {
$(for CASE in "${ITEM_CASES[@]}" ; do echo "        $CASE" ; done)
      }
    }
  }
}
EOT
) > "${NODE_SRC}/${BASE_NAME}Cases.scala"
