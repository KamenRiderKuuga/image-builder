# 使用官方 emqx 镜像作为基础镜像
FROM emqx/emqx:5.3.0

# 复制本地 configs 文件夹到镜像的 /opt/emqx/data 目录
COPY --chown=emqx:emqx ./data /opt/emqx/data
COPY --chown=emqx:emqx ./etc /opt/emqx/etc