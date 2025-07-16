(ns processador-crm.core
  (:require [dk.ative.docjure.spreadsheet :as ss]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

;; --- ETAPA 1: CONFIGURAÇÃO ---
(def arquivo-entrada "base_teste.xlsx") ; Pode ser a sua base sem títulos
(def arquivo-saida "saida_para_crm_inferida.csv")

;; O mapa de modelos agora serve como um dicionário para o nosso detetive
(def mapa-modelos
  {"strada" "Strada", "onix" "Onix", "gol" "Gol", "hb20" "HB20",
   "compass" "Compass", "kwid" "Kwid", "t-cross" "T-Cross", "jeep" "Jeep",
   "fiat" "Fiat", "chevrolet" "Chevrolet", "vw" "VW", "renault" "Renault"})

;; --- ETAPA 2: FUNÇÕES DE TRANSFORMAÇÃO (as mesmas de antes) ---

(defn extrair-primeiro-nome [nome-completo]
  (when (and nome-completo (not (str/blank? nome-completo)))
    (first (str/split (str nome-completo) #"\s+"))))

(defn padronizar-telefone [telefone-str]
  (when (and telefone-str (not (str/blank? telefone-str)))
    (str/replace (str telefone-str) #"[^\d]" "")))

(defn simplificar-modelo [descricao]
  (let [descricao-lower (some-> descricao str/lower)]
    (if descricao-lower
      (or (some (fn [[chave valor]] (when (str/includes? descricao-lower chave) valor))
                mapa-modelos)
          "Modelo não identificado")
      nil)))

;; --- ETAPA 3: OS DETETIVES (HEURÍSTICAS) ---

;; Detetive 1: Um valor se parece com um telefone?
(defn parece-telefone? [valor]
  (let [limpo (padronizar-telefone valor)]
    (and limpo (<= 10 (count limpo) 13)))) ; Telefones no Brasil têm de 10 a 13 dígitos (com +55)

;; Detetive 2: Um valor se parece com um nome?
(defn parece-nome? [valor]
  (and (string? valor)
       (re-matches #"^[a-zA-ZÀ-ú\s\.]+$" valor) ; Letras, acentos, espaços, pontos
       (> (count (str/split valor #"\s+")) 1))) ; Tende a ter mais de uma palavra

;; Detetive 3: Um valor contém uma palavra-chave de carro?
(defn contem-keyword-carro? [valor]
  (let [descricao-lower (some-> valor str/lower)]
    (and descricao-lower
         (some #(str/includes? descricao-lower (key %)) mapa-modelos))))

;; Função que analisa uma coluna inteira e retorna uma pontuação de 0.0 a 1.0
(defn score-coluna [funcao-detetive coluna]
  (let [amostra (take 50 coluna) ; Analisa no máximo 50 itens para ser rápido
        acertos (count (filter funcao-detetive amostra))]
    (/ acertos (double (count amostra)))))

;; Cérebro da operação: encontra o índice da melhor coluna para cada tipo
(defn identificar-indices-auto [colunas]
  (let [scores (for [coluna colunas]
                 {:telefone (score-coluna parece-telefone? coluna)
                  :nome (score-coluna parece-nome? coluna)
                  :carro (score-coluna contem-keyword-carro? coluna)})

        ;; Encontra o índice da coluna com a maior pontuação para cada tipo
        find-best-idx (fn [tipo]
                        (->> scores
                             (map-indexed vector)
                             (apply max-key #(get-in % [1 tipo]))
                             first))]
    {:telefone (find-best-idx :telefone)
     :primeiro-nome (find-best-idx :nome)
     :modelo-carro (find-best-idx :carro)}))

(defn solicitar-indices-manual []
  (println "\n--- Modo Manual ---")
  (println "Por favor, insira o número da coluna (começando em 0) para cada campo:")
  (let [ler-indice (fn [campo]
                     (print (str campo ": "))
                     (flush)
                     (Integer/parseInt (read-line)))]
    {:telefone (ler-indice "Telefone")
     :primeiro-nome (ler-indice "Primeiro Nome")
     :modelo-carro (ler-indice "Modelo do Carro")}))


;; --- ETAPA 4: LÓGICA PRINCIPAL ADAPTADA ---

(defn -main [& args]
  (try
    (println "Bem-vindo ao Processador de CRM!")
    (println "Escolha o modo de operação:")
    (println "1. Automático (detectar colunas)")
    (println "2. Manual (informar colunas)")
    (print "Opção: ")
    (flush)

    (let [modo (read-line)
          ;; Lê a planilha sem assumir cabeçalho
          dados-brutos (->> (ss/load-workbook arquivo-entrada)
                            (ss/select-sheet (first (ss/sheet-names *1)))
                            (ss/read-sheet-data))
          ;; Transpõe os dados para analisar as colunas
          colunas (apply mapv vector dados-brutos)

          ;; Escolhe a estratégia de identificação de índices
          indices (case modo
                    "1" (let [indices-auto (identificar-indices-auto colunas)]
                          (println (str "\nÍndices inferidos automaticamente: " indices-auto))
                          indices-auto)
                    "2" (solicitar-indices-manual)
                    (println "Opção inválida. Usando modo automático por padrão."
                             (let [indices-auto (identificar-indices-auto colunas)]
                               (println (str "\nÍndices inferidos automaticamente: " indices-auto))
                               indices-auto)))]

      (println (str "Índices a serem utilizados: " indices))

      ;; Processa os dados brutos usando os índices descobertos
      (let [dados-processados (map (fn [linha]
                                     {:telefone (padronizar-telefone (get linha (:telefone indices)))
                                      :primeiro-nome (extrair-primeiro-nome (get linha (:primeiro-nome indices)))
                                      :modelo-carro (simplificar-modelo (get linha (:modelo-carro indices)))})
                                   dados-brutos)

          cabecalho [:telefone :primeiro-nome :modelo-carro]
          linhas (map (apply juxt cabecalho) dados-processados)]

      (with-open [writer (io/writer arquivo-saida)]
        (csv/write-csv writer (cons (map name cabecalho) linhas)))

      (println (str "\nArquivo '" arquivo-saida "' criado com sucesso!")))

    (catch Exception e
      (println (str "Ocorreu um erro inesperado: " (.getMessage e) "\n" (pr-str e))))))
