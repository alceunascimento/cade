import requests
from bs4 import BeautifulSoup
from googlesearch import search
import os

# Lista de números de processos

processos = [
    "08012.005324/2012-59", "08700.008576/2012-81", "08700.009509/2012-84",
    "08700.010979/2013-71", "08700.004012/2014-31", "08700.005789/2014-13",
    "08700.007247/2014-85", "08700.009161/2014-97", "08700.010056/2014-09",
    "08700.011474/2014-05", "08700.000448/2015-32", "08700.000949/2015-19",
    "08700.003735/2015-02", "08700.004633/2015-04", "08700.005146/2015-51",
    "08700.007052/2015-16", "08700.009029/2015-66", "08700.009167/2015-45",
    "08700.001094/2016-24", "08700.001281/2017-99", "08700.001783/2017-10",
    "08700.002904/2017-41", "08700.002938/2017-35", "08700.006006/2017-61",
    "08700.003855/2018-44", "08700.000171/2019-71", "08700.000881/2019-00",
    "08700.002070/2019-35", "08700.002290/2019-69", "08700.002787/2019-87",
    "08700.003910/2019-87", "08700.004287/2019-80", "08700.006005/2019-89",
    "08700.005714/2020-81", "08700.002012/2021-26", "08700.001825/2022-80",
    "08700.000478/2024-30"
]

# processos = [ "08700.006005/2019-89" ]


# Função para fazer a busca no Google
def search_processos(query, num_results=10):
    results = []
    for result in search(query, num_results=num_results):
        if "sei.cade.gov.br" in result:
            results.append(result)
    return results

# Função para baixar o conteúdo HTML dos processos
def download_html(process_urls, save_folder="processos_html"):
    if not os.path.exists(save_folder):
        os.makedirs(save_folder)
        
    for url in process_urls:
        try:
            response = requests.get(url)
            if response.status_code == 200:
                soup = BeautifulSoup(response.content, 'html.parser')
                title = soup.title.string.strip().replace(" ", "_").replace("/", "_")
                file_path = os.path.join(save_folder, f"{title}.html")
                
                # Salvando o HTML
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(str(soup))
                print(f"Downloaded: {title}")
            else:
                print(f"Failed to download: {url} - Status code: {response.status_code}")
        except Exception as e:
            print(f"Error downloading {url}: {e}")

# Loop para buscar os processos no Google e baixar os HTMLs
for processo in processos:
    search_query = f'site:https://sei.cade.gov.br/sei/modulos/pesquisa/md_pesq_documento_consulta_externa.php? "{processo}"'
    process_links = search_processos(search_query, num_results=5)
    download_html(process_links)
    time.sleep(10)



