(require 'leanote)
(describe "test leanote-notebook-has-note-p"
          :var (notes)
          (it "test"
              (setq notes '[((PublicTime . "0001-01-01T00:00:00Z")
                             (UpdatedTime . "2016-07-23T20:57:20.651474234+08:00")
                             (CreatedTime . "0001-01-01T00:00:00Z")
                             (Files)
                             (Usn . 151)
                             (IsDeleted . :json-false)
                             (IsTrash . :json-false)
                             (IsBlog . :json-false)
                             (IsMarkdown . t)
                             (Content . "")
                             (Abstract . "# abc\n这是测试文件\n\n## 二级目录\n1. a\n2. b\n3. c\n\n### 第三级目录\n第三级内容\n\n## 另一个二级目录\n* a\n* bb\n* cc\n* dd\n")
                             (Tags)
                             (Desc . "")
                             (Title . "abc")
                             (UserId . "5781be6dab644133ed006e61")
                             (NotebookId . "57899e70c3b1f40b51000007")
                             (NoteId . "5793514eab644133ed01f5be"))])
              (expect (leanote-notebook-has-note-p notes "abc")
                      :to-be t)
              (expect (leanote-notebook-has-note-p notes "ddd")
                      :to-be nil)))

(describe "test leanote-extra-abstract"
          (it "test"
              (let* ((s1 " # 工作备忘\n主要一些当前正在进行的一些工作\n\n## 进行中\n* 重新加入队列(再次取的时候是通过unionid来处理，这里要考虑)\n* 队列数据监控\n* 对活跃消费者线程数进行监控\n* retryCounter重试次数数据保存到哪里？\n\n## 问题\n1. 每个appid是否需要一个独立的工作队列和消费队列？保证不相互影响！\n2. tomcat初始化的时候，是否要对每个队列激活消费者？暂时保持不需要\n\n")
                     (s2 "# 工作备忘\n主要一些当前正在进行的一些工作\n\n## 进行中\n* 重新加入队列(再次取的时候是通过unionid来处理，这里要考虑)\n* 队列数据监控\n* 对活跃消费者线程数进行监控\n* retryCounter重试次数数据保存到哪里？\n\n## 问题\n1. 每个appid是否需要一个独立的工作队列和消费队列？保证不相互影响！\n2. tomcat初始化的时候，是否要对每个队列激活消费者？暂时保持不需要\n\n")
                     (s3 "#工作备忘\n")
                     (s4 "#工作备忘\n#")
                     (s5 "#工作备忘\n##ff")
                     )
                (expect (leanote-extra-abstract s3)
                        :to-equal "工作备忘")
                (expect (leanote-extra-abstract s4)
                        :to-equal "工作备忘")
                (expect (leanote-extra-abstract s5)
                        :to-equal "工作备忘")
                (expect (leanote-extra-abstract s1)
                        :to-equal "主要一些当前正在进行的一些工作")
                (expect (leanote-extra-abstract s2)
                        :to-equal "主要一些当前正在进行的一些工作"))))

(describe "test leanote-status-is-timeout"
          (it "test"
              (let* ((status1 '("578e2182ab644133ed01800b" t
                                (22437 21873 142076 0))))
                (expect (leanote-status-is-timeout status1)
                        :to-be t)
                )))

(describe "test leanote--path-without-slash"
          (it "test"
              (let* ((path1 "/abd")
                     (path2 "/abcd/")
                     (path3 "中/")
                     (path4 "/"))
                (expect (leanote--path-without-slash path1) :to-equal path1)
                (expect (leanote--path-without-slash path2) :to-equal "/abcd")
                (expect (leanote--path-without-slash path3) :to-equal "中")
                (expect (leanote--path-without-slash path4) :to-equal "")
                )))
